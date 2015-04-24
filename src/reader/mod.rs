//! Contains high-level interface for a pull-based XML parser.
//!
//! The most important type in this module is `EventReader`, which provides an iterator
//! view for events in XML document.

use std::io::prelude::*;

use common::{Position, TextPosition};
use self::parser::PullParser;
use self::events::XmlEvent;

pub use self::config::ParserConfig;

mod lexer;
mod parser;
pub mod config;
pub mod events;

/// Simple wrapper around an `std::io::BufReader` which provides pull-based XML parsing.
pub struct EventReader<B: Read> {
    source: B,
    parser: PullParser
}

impl<B: Read> EventReader<B> {
    /// Creates a new parser, consuming given stream.
    #[inline]
    pub fn new(source: B) -> EventReader<B> {
        EventReader::with_config(source, ParserConfig::new())
    }

    /// Creates a new parser with the provded configuration, consuming given `Buffer`.
    #[inline]
    pub fn with_config(source: B, config: ParserConfig) -> EventReader<B> {
        EventReader { source: source, parser: PullParser::new(config) }
    }

    /// Pulls and returns next XML event from the stream.
    ///
    /// If returned event is `xml::event::Error` or `xml::event::EndDocument`, then
    /// further calls to this method will return this event again.
    #[inline]
    pub fn next(&mut self) -> XmlEvent {
        self.parser.next(&mut self.source)
    }

    /// Returns an iterator over XML events.
    ///
    /// When the next event is `xml::event::Error` or `xml::event::EndDocument`, then
    /// it will be returned by the iterator once, and then it will stop producing events.
    #[inline]
    pub fn events<'a>(&'a mut self) -> Events<'a, B> {
        Events { reader: self, finished: false }
    }
}

impl<B: Read> Position for EventReader<B> {
    /// Returns the position of the last event produced by the parser
    #[inline]
    fn position(&self) -> TextPosition {
        self.parser.position()
    }
}

/// XML events iterator, created by `events()` method on `Parser`.
pub struct Events<'a, B: Read+'a> {
    reader: &'a mut EventReader<B>,
    finished: bool
}

impl<'a, B: Read> Iterator for Events<'a, B> {
    type Item = XmlEvent;

    #[inline]
    fn next(&mut self) -> Option<XmlEvent> {
        if self.finished { None }
        else {
            let ev = self.reader.next();
            match ev {
                XmlEvent::EndDocument | XmlEvent::Error(_) => self.finished = true,
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
