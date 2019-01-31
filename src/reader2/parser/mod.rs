use std::io::Read;

use crate::event::XmlEvent;
use crate::position::{TextPosition, Position};
use super::error::Result;
use super::{DelimitingReader, ParserConfig, Buffer};

mod attributes;
mod prolog;
mod doctype;
mod util;
mod comment;
mod start_element;

pub struct Parser<R: Read> {
    config: ParserConfig,
    source: DelimitingReader<R>,
    buffer: String,
    state: State,
    pos: TextPosition,
}

impl<R: Read> Position for Parser<R> {
    fn position(&self) -> TextPosition {
        self.pos
    }
}

enum State {
    Prolog(PrologSubstate),
    OutsideTag,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum PrologSubstate {
    BeforeDeclaration,
    BeforeDoctype,
    BeforeDocument,
}

const BUFFER_SIZE: usize = 8192;

impl<R: Read> Parser<R> {
    pub fn new(config: ParserConfig, source: R) -> Parser<R> {
        Parser {
            config,
            source: DelimitingReader::new(source, BUFFER_SIZE),
            buffer: String::new(),
            state: State::Prolog(PrologSubstate::BeforeDeclaration),
            pos: TextPosition::new(),
        }
    }

    pub fn next<'buf>(&mut self, buffer: &'buf mut Buffer) -> Result<XmlEvent<'buf>> {
        buffer.clear();

        let buffer_ptr = buffer as *mut _;
        loop {
            // Unsafe is bad, but this *is* safe

            let buffer = unsafe { &mut *buffer_ptr };
            let temp_event = match self.state {
                State::Prolog(substate) => self.parse_prolog(substate, buffer),
                State::OutsideTag => self.parse_outside_tag(buffer),
            };

            match temp_event {
                Ok(XmlEvent::Comment(_)) if self.config.ignore_comments => continue,
                _ => return temp_event,
            }
        }
    }

    fn parse_outside_tag<'buf>(&mut self, buffer: &'buf mut Buffer) -> Result<XmlEvent<'buf>> {
        // At this point: buffer is empty

        unimplemented!()
    }

    fn parse_processing_instruction<'buf>(&mut self, buffer: &'buf mut Buffer) -> Result<XmlEvent<'buf>> {
        // At this point: buffer == '[whitespace]<?xxx'

        unimplemented!()
    }
}
