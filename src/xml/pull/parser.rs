use std::char;

use common::{is_name_char, is_whitespace};
use events::{XmlEvent, EndDocument, Error};

pub struct PullParser {
    priv row: uint,
    priv col: uint,
    priv st: State,
    priv buf: ~str,

    priv processed_element: bool
    priv inside_whitespace: bool
}

pub fn new() -> PullParser {
    PullParser {
        row: 0,
        col: 0,
        st: OutsideTag,
        buf: ~[],

        processed_element: false,
        inside_whitespace: true
    }
}

#[deriving(Clone, Default)]
enum State {
    OutsideTag,
    TagStarted,
    InsideOpeningTag(Substate),
    InsideClosingTagName,
    InsideProcessingInstruction(Substate),
    InsideProlog(Substate)
}

#[deriving(Clone, Default)]
enum Substate {
    InsideName,
    InsideAttributeName,
    InsideAttributeValue,
    InsideData
}

macro_rules! parse_error(
    ($fmt:expr, $($arg:expr),+) => (
        self.error(format!($fmt, $($arg),+))
    )
)

impl PullParser {
    pub fn next<B: Buffer>(&mut self, r: &mut B) -> XmlEvent {
        foreach!(c in r.read_char() {
            if c == '\n' {
                self.row += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }

            match self.parse_char(c) {
                Some(ev) => return ev,
                None => {}  // continue
            }
        })
        self.error(~"Unexpected end of stream")
    }

    fn error(&self, msg: ~str) -> XmlEvent {
        Error { row: self.row+1, col: self.col+1, msg: msg }
    }

    fn parse_char(&mut self, c: char) -> Option<XmlEvent> {
        match self.st {
            OutsideTag                     => self.outside_tag(c),
            TagStarted                     => self.tag_started(c),
            InsideProcessingInstruction(s) => self.inside_processing_instruction(c, s),
            InsideOpeningTag(s)            => self.inside_opening_tag(c, s)
        }
    }

    fn outside_tag(&mut self, c: char) -> Option<XmlEvent> {
        match c {
            '<' if self.buf.len() > 0 => {
                let buf = self.buf.clone();
                self.buf.clear();

                let result = if self.inside_whitespace {
                    Whitespace(buf)
                } else {
                    Characters(buf)
                };

                self.inside_whitespace = true;
                self.st = TagStarted;

                result
            }
            '<' => {
                self.st = TagStarted;
                None
            },
            _ => {
                if !char::is_whitespace(c) {
                    self.inside_whitespace = false;
                }
                self.buf.push_char(c);
                None
            }
        }
    }

    fn tag_started(&mut self, c: char) -> Option<XmlEvent> {
        match c {
            '?' => {
                self.st = InsideProcesssingInstruction(Name);
                None
            }
            _ => {
                self.st = InsideOpeningTag(Name);
                self.buf.push_char(c);
                None
            }
        }
    }

    fn inside_processing_instruction(&mut self, c: char, s: Substate) -> Option<XmlEvent> {
        match s {
            Name => match c {
                _ if is_name_char(c) => {
                     self.buf.push_char(c);
                }
                _ if is_whitespace(c) => {
                    self.
                }
            }
 
            Data => {
            }
            _ => parse_error!("Unexpected substate inside processing instruction: {}", s)
        }
    }
}
