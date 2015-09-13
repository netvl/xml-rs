//! Contains high-level interface for a pull-based XML parser.
//!
//! The most important type in this module is `EventReader`, which provides an iterator
//! view for events in XML document.

use std::io::Read;
use std::borrow::Cow;
use std::result;
use std::fmt;
use std::error;

use common::{Position, TextPosition};
use reader::parser::PullParser;

pub use reader::config::ParserConfig;
pub use reader::events::XmlEvent;

mod lexer;
mod parser;
mod config;
mod events;

/// XML parsing error.
///
/// Consists of a position and a message.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error {
    pos: TextPosition,
    msg: Cow<'static, str>
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.pos, self.msg)
    }
}

impl Position for Error {
    #[inline]
    fn position(&self) -> TextPosition { self.pos }
}

impl Error {
    /// Creates a new error using position information from the provided
    /// `Position` object and a message.
    #[inline]
    pub fn new<O: Position, S: Into<Cow<'static, str>>>(o: &O, msg: S) -> Error {
        Error { pos: o.position(), msg: msg.into() }
    }

    /// Returns a reference to a message which is contained inside this error.
    #[inline]
    pub fn msg(&self) -> &str { &self.msg }
}

impl error::Error for Error {
    #[inline]
    fn description(&self) -> &str { &*self.msg }
}

pub type Result<T> = result::Result<T, Error>;

/// A wrapper around an `std::io::Reader` which provides pull-based XML parsing.
pub struct EventReader<R: Read> {
    source: R,
    parser: PullParser
}

impl<R: Read> EventReader<R> {
    /// Creates a new reader, consuming given stream.
    #[inline]
    pub fn new(source: R) -> EventReader<R> {
        EventReader::new_with_config(source, ParserConfig::new())
    }

    /// Creates a new reader with the provded configuration, consuming given `Buffer`.
    #[inline]
    pub fn new_with_config(source: R, config: ParserConfig) -> EventReader<R> {
        EventReader { source: source, parser: PullParser::new(config) }
    }

    /// Pulls and returns next XML event from the stream.
    ///
    /// If returned event is `XmlEvent::Error` or `XmlEvent::EndDocument`, then
    /// further calls to this method will return this event again.
    #[inline]
    pub fn next(&mut self) -> Result<XmlEvent> {
        self.parser.next(&mut self.source)
    }
}

impl<B: Read> Position for EventReader<B> {
    /// Returns the position of the last event produced by the reader.
    #[inline]
    fn position(&self) -> TextPosition {
        self.parser.position()
    }
}

impl<R: Read> IntoIterator for EventReader<R> {
    type Item = Result<XmlEvent>;
    type IntoIter = Events<R>;

    fn into_iter(self) -> Events<R> {
        Events { reader: self, finished: false }
    }
}

/// An iterator over XML events created from some type implementing `Read`.
///
/// When the next event is `xml::event::Error` or `xml::event::EndDocument`, then
/// it will be returned by the iterator once, and then it will stop producing events.
pub struct Events<R: Read> {
    reader: EventReader<R>,
    finished: bool
}

impl<R: Read> Events<R> {
    #[inline]
    pub fn into_inner(self) -> EventReader<R> {
        self.reader
    }
}

impl<R: Read> Iterator for Events<R> {
    type Item = Result<XmlEvent>;

    #[inline]
    fn next(&mut self) -> Option<Result<XmlEvent>> {
        if self.finished { None }
        else {
            let ev = self.reader.next();
            match ev {
                Ok(XmlEvent::EndDocument) | Err(_) => self.finished = true,
                _ => {}
            }
            Some(ev)
        }
    }
}

impl<'r> EventReader<&'r [u8]> {
    /// Convenience method to create a reader from a string slice.
    #[inline]
    pub fn from_str(source: &'r str) -> EventReader<&'r [u8]> {
        EventReader::new(source.as_bytes())
    }
}
