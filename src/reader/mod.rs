//! Contains high-level interface for a pull-based XML parser.
//!
//! The most important type in this module is `EventReader`, which provides an iterator
//! view for events in XML document.

use std::io::{self, Read};
use std::borrow::Cow;
use std::result;
use std::str;

use common::{Position, TextPosition};

pub use self::config::ParserConfig;
pub use self::events::XmlEvent;

use self::parser::PullParser;

mod lexer;
mod parser;
mod config;
mod events;

mod error_impl;

#[derive(Debug)]
pub enum ErrorKind {
    Syntax( Cow<'static, str> ),
    Io( io::Error ),
    Utf8( str::Utf8Error ),
    UnexpectedEof,
}

/// An XML parsing error.
///
/// Consists of a 2D position in a document and a textual message describing the error.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error {
    pos: TextPosition,
    kind: ErrorKind,
}

/// A result type yielded by `XmlReader`.
pub type Result<T> = result::Result<T, Error>;

/// A wrapper around an `std::io::Read` instance which provides pull-based XML parsing.
pub struct EventReader<R: Read> {
    source: R,
    parser: PullParser
}

impl<R: Read> EventReader<R> {
    /// Creates a new reader, consuming the given stream.
    #[inline]
    pub fn new(source: R) -> EventReader<R> {
        EventReader::new_with_config(source, ParserConfig::new())
    }

    /// Creates a new reader with the provded configuration, consuming the given stream.
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
    /// Unwraps the iterator, returning the internal `EventReader`.
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
    /// A convenience method to create an `XmlReader` from a string slice.
    #[inline]
    pub fn from_str(source: &'r str) -> EventReader<&'r [u8]> {
        EventReader::new(source.as_bytes())
    }
}
