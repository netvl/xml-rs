use std::collections::VecDeque;
use std::io;

use nom::error::VerboseError;

use crate::event::Event;
use crate::reader::data::{Buffer, StrRead};
use crate::reader::error::{Error, Result};
use crate::reader::model;
use crate::reader::ReaderConfig;

use self::logic::{ParserLogic, ParserLogicError};
use crate::utils::position::{Position, TextPosition};
use std::cell::Cell;

mod logic;

pub struct Reader<R: StrRead> {
    source: R,
    config: ReaderConfig,
    logic: ParserLogic,
    buffer: Buffer,
    pos: usize,
    next_events: VecDeque<model::CowEvent>,
    current_event: Cell<Option<model::CowEvent>>,
}

impl<R: StrRead> Reader<R> {
    pub fn new(config: ReaderConfig, source: R) -> Reader<R> {
        Reader {
            source,
            config: config.clone(),
            logic: ParserLogic::new(config),
            buffer: Buffer::new(),
            pos: 0,
            next_events: VecDeque::new(),
            current_event: Cell::new(None),
        }
    }

    pub fn next(&mut self) -> Result<Event> {
        self.advance()?;
        // Now take_current() is guaranteed to finish correctly
        Ok(self.take_current().unwrap())
    }

    pub fn take_current(&self) -> Option<Event> {
        let result = self.current_event.replace(None);
        let mut result = result.map(|e| e.reify(&self.buffer));
        if let Some(result) = result.as_mut() {
            result.resolve_namespaces(self.logic.current_namespaces());
        }
        result
    }

    pub fn advance(&mut self) -> Result<()> {
        if let Some(event) = self.next_events.pop_front() {
            return self.return_event(event);
        }

        // TODO: Once in a while, we must clear the internal buffer for it not to grow indefinitely.
        //       To do this, we should go through all our in-memory structures pointing to the buffer (e.g. namespaces,
        //       the stack of element names, etc), reify them in-place and then clear the buffer.

        let buffer = &mut *self.buffer as *mut _;

        let event = loop {
            let slice = &self.buffer[self.pos..];
            match self.logic.try_next::<VerboseError<_>>(&self.buffer, slice) {
                Ok(parsed) => {
                    self.pos += parsed.bytes_read;
                    self.next_events.extend(parsed.into_iter());

                    if self.should_continue_reading() {
                        continue;
                    }

                    break self.compute_next_event();
                }

                // TODO: limit reading more data? Otherwise memory overflow is possible for
                //       large documents
                Err(ParserLogicError::Incomplete(_)) => {
                    let buffer = unsafe { &mut *buffer };

                    let position = self.position();
                    if self
                        .source
                        .will_increase_capacity(buffer)
                        .map_err(|e| Error::new(e, position))?
                    {
                        for event in &mut self.next_events {
                            event.reify_in_place(&self.buffer);
                        }
                        self.logic.reify_state(&self.buffer);
                    }

                    if !self.source.read_str_data(buffer).map_err(|e| Error::new(e, position))? {
                        if self.logic.is_document_correctly_closed() {
                            return self.return_event(Event::end_document());
                        }

                        return Err(Error::new(
                            io::Error::new(io::ErrorKind::UnexpectedEof, "Unexpected end of document"),
                            self.position(),
                        ));
                    }
                }

                Err(ParserLogicError::Parsing(e)) => {
                    let e = VerboseError {
                        errors: e.errors.into_iter().map(|(span, vek)| (span.fragment, vek)).collect(),
                    };
                    return Err(Error::new((slice, e), self.position()));
                }
                Err(ParserLogicError::Logic(e)) => return Err(Error::new(e.into_owned(), self.position())),
            }
        };

        self.return_event(event)
    }

    pub fn fused(self) -> FusedReader<R> {
        FusedReader {
            inner: self,
            encountered_end_or_error: false,
        }
    }

    fn return_event(&mut self, event: impl Into<model::CowEvent>) -> Result<()> {
        let event = event.into();
        if event.is_end_element() {
            self.logic.pop_namespace_before_next_event();
        }
        self.current_event.set(Some(event));
        Ok(())
    }

    fn should_continue_reading(&self) -> bool {
        last_event_is_text(&self.next_events) && self.config.coalesce_characters
    }

    fn compute_next_event(&mut self) -> model::CowEvent {
        if next_event_is_text(&self.next_events) && self.config.coalesce_characters {
            let mut result = self.next_events.pop_front().unwrap();
            while next_event_is_text(&self.next_events) {
                // Guaranteed to be Event::Text
                let next_event = self.next_events.pop_front().unwrap();
                merge_text(&mut result, next_event, &self.buffer);
            }
            result
        } else {
            self.next_events
                .pop_front()
                .expect("Implementation error: ParserLogic::try_next() returned empty output")
        }
    }
}

impl<R: StrRead> Position for Reader<R> {
    fn position(&self) -> TextPosition {
        self.logic.position()
    }
}

pub struct FusedReader<R: StrRead> {
    inner: Reader<R>,
    encountered_end_or_error: bool,
}

impl<R: StrRead> FusedReader<R> {
    pub fn next(&mut self) -> Option<Result<Event>> {
        if self.encountered_end_or_error {
            None
        } else {
            let event = self.inner.next();
            match event {
                Ok(Event::EndDocument) | Err(_) => {
                    self.encountered_end_or_error = true;
                }
                _ => {}
            }
            Some(event)
        }
    }

    pub fn into_inner(self) -> Reader<R> {
        self.inner
    }
}

impl<R: StrRead> Position for FusedReader<R> {
    fn position(&self) -> TextPosition {
        self.inner.position()
    }
}

fn merge_text(target: &mut model::CowEvent, source: model::CowEvent, buffer: &Buffer) {
    use crate::reader::model::CowEvent::{Ephemeral, Reified};

    debug_assert!(target.is_text());
    debug_assert!(source.is_text());

    match target {
        Ephemeral(t) => match source {
            Ephemeral(s) => {
                let (t_text, s_text) = (t.as_text_ref_mut(), s.as_text());
                if t_text.directly_precedes(&s_text) {
                    *t_text = t_text.merge_with_following(&s_text);
                } else {
                    target.reify_in_place(buffer);
                    merge_text(target, Ephemeral(s), buffer);
                }
            }
            Reified(s) => {
                target.reify_in_place(buffer);
                merge_text(target, Reified(s), buffer);
            }
        },
        Reified(t) => match source {
            Ephemeral(s) => t.as_text_mut().to_mut().push_str(s.as_text().as_reified(buffer)),
            Reified(s) => t.as_text_mut().to_mut().push_str(s.as_text()),
        },
    }
}

fn last_event_is_text(events: &VecDeque<model::CowEvent>) -> bool {
    events.back().map(|e| e.is_text()) == Some(true)
}

fn next_event_is_text(events: &VecDeque<model::CowEvent>) -> bool {
    events.front().map(|e| e.is_text()) == Some(true)
}
