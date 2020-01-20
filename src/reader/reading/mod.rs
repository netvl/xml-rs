use std::borrow::Cow;
use std::collections::VecDeque;
use std::io;

use arraydeque::ArrayDeque;
use nom::error::{ParseError, VerboseError};
use nom::{Err, IResult, Needed};

use crate::event::{Event, XmlVersion};
use crate::reader::data::{BufSlice, Buffer, StrRead};
use crate::reader::error::{Error, Result};
use crate::reader::model;
use crate::reader::ReaderConfig;
use parsers::{Parsed, ParsedHint, ReferenceHint, StartTagHint};

mod parsers;

pub struct Reader<R: StrRead> {
    source: R,
    config: ReaderConfig,
    logic: ParserLogic,
    buffer: Buffer,
    pos: usize,
    next_events: VecDeque<model::CowEvent>,
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
        }
    }

    pub fn next(&mut self) -> Result<Event> {
        if let Some(event) = self.next_events.pop_front() {
            return Ok(event.reify(&self.buffer));
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

                    if self.source.will_increase_capacity(buffer)? {
                        for event in &mut self.next_events {
                            event.reify_in_place(&self.buffer);
                        }
                        self.logic.reify_state(&self.buffer);
                    }

                    if !self.source.read_str_data(buffer)? {
                        if self.logic.encountered_element && self.logic.open_elements.is_empty() {
                            return Ok(Event::end_document());
                        }

                        return Err(Error::from(io::Error::new(
                            io::ErrorKind::UnexpectedEof,
                            "Unexpected end of document",
                        )));
                    }
                }

                Err(ParserLogicError::Parsing(e)) => return Err(Error::from((slice, e))),
                Err(ParserLogicError::Logic(e)) => return Err(Error::from(e.into_owned())),
            }
        };

        Ok(event.reify(&self.buffer))
    }

    pub fn fused(self) -> FusedReader<R> {
        FusedReader {
            inner: self,
            encountered_end_or_error: false,
        }
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
enum State {
    Prolog(PrologSubstate),
    OutsideTag,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
enum PrologSubstate {
    BeforeDeclaration,
    BeforeDoctype,
    BeforeDocument,
}

#[derive(Debug)]
struct ParserLogicOutput {
    bytes_read: usize,
    events: ArrayDeque<[model::CowEvent; 3]>,
}

impl ParserLogicOutput {
    fn new(bytes_read: usize) -> ParserLogicOutput {
        ParserLogicOutput {
            bytes_read,
            events: ArrayDeque::new(),
        }
    }

    fn push_front(&mut self, event: impl Into<model::CowEvent>) {
        self.events
            .push_front(event.into())
            .expect("Implementation error: too many events are pushed to the parser output");
    }

    fn push_back(&mut self, event: impl Into<model::CowEvent>) {
        self.events
            .push_back(event.into())
            .expect("Implementation error: too many events are pushed to the parser output");
    }

    fn into_iter(self) -> impl Iterator<Item = model::CowEvent> {
        self.events.into_iter()
    }
}

enum ParserLogicError<E> {
    Incomplete(Needed),
    Parsing(E),
    Logic(Cow<'static, str>),
}

impl<E> From<Err<E>> for ParserLogicError<E> {
    fn from(e: Err<E>) -> Self {
        match e {
            Err::Error(e) | Err::Failure(e) => ParserLogicError::Parsing(e),
            Err::Incomplete(n) => ParserLogicError::Incomplete(n),
        }
    }
}

impl<E> From<String> for ParserLogicError<E> {
    fn from(s: String) -> Self {
        ParserLogicError::Logic(s.into())
    }
}

impl<E> From<&'static str> for ParserLogicError<E> {
    fn from(s: &'static str) -> Self {
        ParserLogicError::Logic(s.into())
    }
}

type ParserLogicResult<E> = std::result::Result<ParserLogicOutput, ParserLogicError<E>>;

pub struct ParserLogic {
    config: ReaderConfig,
    state: State,
    encountered_declaration: bool,
    encountered_element: bool,
    open_elements: Vec<model::CowName>,
}

impl ParserLogic {
    fn new(config: ReaderConfig) -> ParserLogic {
        ParserLogic {
            config,
            state: State::Prolog(PrologSubstate::BeforeDeclaration),
            encountered_declaration: false,
            encountered_element: false,
            open_elements: Vec::new(),
        }
    }

    fn reify_state(&mut self, buffer: &Buffer) {
        for name in &mut self.open_elements {
            name.reify_in_place(buffer);
        }
    }

    fn try_next<'buf, E>(&mut self, buffer: &'buf Buffer, input: &'buf str) -> ParserLogicResult<E>
    where
        E: ParseError<&'buf str>,
    {
        let result = match self.state {
            State::Prolog(substate) => self.parse_prolog(input, substate),
            State::OutsideTag => self.parse_outside_tag(input),
        };
        let (remainder, Parsed { event, hint }) = result?;

        let mut output = ParserLogicOutput::new(input.len() - remainder.len());

        let event = match event {
            model::Event::Whitespace(data) if self.config.whitespace_to_text => model::Event::text(data),
            model::Event::CData(data) if self.config.cdata_to_text => model::Event::text(data),
            other => other,
        };

        match event {
            event @ model::Event::StartDocument { .. } => {
                self.encountered_declaration = true;
                self.state = State::Prolog(PrologSubstate::BeforeDoctype);
                output.push_front(event);
            }

            event @ model::Event::DoctypeDeclaration { .. } => {
                self.state = State::Prolog(PrologSubstate::BeforeDocument);
                output.push_front(event);

                if !self.encountered_declaration {
                    self.encountered_declaration = true;
                    output.push_front(model::Event::start_document(
                        XmlVersion::Version10,
                        BufSlice::new_static("UTF-8"),
                        None,
                    ));
                }
            }

            event @ model::Event::StartElement { .. } => {
                match hint {
                    ParsedHint::StartTag(StartTagHint::EmptyElementTag) => {
                        output.push_back(model::Event::end_element(event.start_element_name()));
                    }
                    ParsedHint::StartTag(StartTagHint::RegularTag) => {
                        self.open_elements.push(event.start_element_name().into());
                    }
                    hint => {
                        debug_assert!(false, "Unexpected ParsedHint: {:?}", hint);
                    }
                }
                self.encountered_element = true;
                self.state = State::OutsideTag;
                output.push_front(event);

                if !self.encountered_declaration {
                    self.encountered_declaration = true;
                    output.push_front(model::Event::start_document(
                        XmlVersion::Version10,
                        BufSlice::new_static("UTF-8"),
                        None,
                    ));
                }
            }

            event @ model::Event::EndElement { .. } => match self.open_elements.pop() {
                Some(open_name) => {
                    let open_name = open_name.as_reified(buffer);
                    let name = event.end_element_name().as_reified(buffer);
                    if name != open_name {
                        return Err(format!("Unexpected closing element '{}', expected '{}'", name, open_name).into());
                    }
                    output.push_front(event);
                }
                None => {
                    return Err("Unexpected closing element, expected an opening element".into());
                }
            },

            event @ model::Event::Comment(_) => {
                // A comment in the header means that no declaration must follow
                self.state = match self.state {
                    State::Prolog(PrologSubstate::BeforeDeclaration) => State::Prolog(PrologSubstate::BeforeDoctype),
                    other => other,
                };
                output.push_front(event);
            }

            event @ model::Event::ProcessingInstruction { .. } => {
                // A PI in the header means that no declaration must follow
                self.state = match self.state {
                    State::Prolog(PrologSubstate::BeforeDeclaration) => State::Prolog(PrologSubstate::BeforeDoctype),
                    other => other,
                };
                output.push_front(event);
            }

            model::Event::CData(data) => {
                if self.config.cdata_to_text {
                    output.push_front(model::Event::Text(data))
                } else {
                    output.push_front(model::Event::CData(data));
                }
            }

            model::Event::Whitespace(data) => {
                if self.config.whitespace_to_text {
                    output.push_front(model::Event::Text(data));
                } else {
                    output.push_front(model::Event::Whitespace(data));
                }
            }

            event @ model::Event::Text(_) => {
                match hint {
                    ParsedHint::Reference(ReferenceHint::Char(ch)) => {
                        output.push_front(Event::Text(ch.to_string().into()));
                    }
                    ParsedHint::Reference(ReferenceHint::Entity(name)) => {
                        // TODO: maybe we should move recognition of built-in entities into the parser below
                        let resolved = match name.as_reified(buffer).local_name.as_ref() {
                            "lt" => '<',
                            "gt" => '>',
                            "amp" => '&',
                            "apos" => '\'',
                            "quot" => '"',
                            r => return Err(format!("Unknown character reference: {}", r).into()),
                        };
                        output.push_front(Event::Text(resolved.to_string().into()));
                    }
                    ParsedHint::None => {
                        // Just regular text
                        output.push_front(event);
                    }
                    hint => {
                        debug_assert!(false, "Unexpected ParsedHint: {:?}", hint);
                    }
                }
            }

            other => todo!("Cannot handle yet: {:?}", other),
        }

        Ok(output)
    }

    fn parse_prolog<'buf, E>(&mut self, input: &'buf str, substate: PrologSubstate) -> IResult<&'buf str, Parsed, E>
    where
        E: ParseError<&'buf str>,
    {
        match substate {
            PrologSubstate::BeforeDeclaration => parsers::before_declaration(input),
            PrologSubstate::BeforeDoctype => parsers::before_doctype(input),
            PrologSubstate::BeforeDocument => parsers::before_document(input),
        }
    }

    fn parse_outside_tag<'buf, E>(&mut self, input: &'buf str) -> IResult<&'buf str, Parsed, E>
    where
        E: ParseError<&'buf str>,
    {
        parsers::outside_tag(input)
    }
}
