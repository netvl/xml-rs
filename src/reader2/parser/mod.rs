use std::io::Read;

use reader2::error::Result;
use reader2::DelimitingReader;
use event::XmlEvent;
use position::{TextPosition, Position};

mod attributes;
mod prolog;
mod doctype;
mod util;

pub struct Parser<R: Read> {
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
    #[cfg(feature = "encodings")]
    pub fn new(source: R) -> Parser<R> {
        Parser {
            source: DelimitingReader::new(source, BUFFER_SIZE),
            buffer: String::new(),
            state: State::Prolog(PrologSubstate::BeforeDeclaration),
            pos: TextPosition::new(),
        }
    }

    pub fn next<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        buffer.clear();
        match self.state {
            State::Prolog(substate) => self.parse_prolog(substate, buffer),
            State::OutsideTag => self.parse_outside_tag(buffer),
        }
    }

    fn parse_outside_tag<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        // At this point: buffer is empty

        unimplemented!()
    }

    fn parse_processing_instruction<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        // At this point: buffer == '[whitespace]<?xxx'

        unimplemented!()
    }

    fn parse_comment<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        // At this point: buffer == '[whitespace]<!-'  <- TODO: verify this

        unimplemented!()
    }

    fn parse_start_element<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        // At this point: buffer == '[whitespace]<'
        unimplemented!()
    }
}
