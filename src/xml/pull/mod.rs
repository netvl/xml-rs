//! Contains high-level interface for a pull-based XML parser.
//!
//! The most important type in this module is `Parser`, which provides an iterator
//! view for events in XML document.

use std::io::Buffer;
use std::io::mem::{MemReader, BufReader};

use events;
use events::XmlEvent;
use self::parser::PullParser;

pub use self::config::ParserConfig;

macro_rules! for_each(
    ($e:ident in $it:expr $body:expr) => (
        loop {
            match $it {
                Some($e) => $body,
                None => break
            }
        }
    )
)

pub mod lexer;
pub mod parser;
pub mod config;

/// Simple wrapper around an `std::io::Buffer` which provides pull-based XML parsing.
pub struct Parser<B> {
    priv source: B,
    priv parser: PullParser
}

impl<B: Buffer> Parser<B> {
    /// Creates a new parser, consuming given `Buffer`.
    #[inline]
    pub fn new(source: B) -> Parser<B> {
        Parser::new_with_config(source, ParserConfig::new())
    }

    /// Creates a new parser with the provded configuration, consuming given `Buffer`.
    #[inline]
    pub fn new_with_config(source: B, config: ParserConfig) -> Parser<B> {
        Parser { source: source, parser: parser::new(config) }
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
        Events { parser: self, finished: false }
    }
}

/// XML events iterator, created by `events()` method on `Parser`.
pub struct Events<'a, B> {
    priv parser: &'a mut Parser<B>,
    priv finished: bool
}

impl<'a, B: Buffer> Iterator<XmlEvent> for Events<'a, B> {
    #[inline]
    fn next(&mut self) -> Option<XmlEvent> {
        if self.finished { None } 
        else {
            let ev = self.parser.next();
            match ev {
                events::EndDocument | events::Error(_) => self.finished = true,
                _ => {}
            }
            Some(ev)
        }
    }
}

impl Parser<MemReader> {
    /// Convenience method to create a parser from an owned string.
    #[inline]
    pub fn new_from_str(source: ~str) -> Parser<MemReader> {
        Parser::new_from_bytes(source.into_bytes())
    }

    /// Convenience method to create a parser from an owned vector of bytes.
    #[inline]
    pub fn new_from_bytes(source: ~[u8]) -> Parser<MemReader> {
        Parser::new(MemReader::new(source))
    }

}

impl<'r> Parser<BufReader<'r>> {
    /// Convenience method to create a parser from a string slice.
    #[inline]
    pub fn new_from_str_slice(source: &'r str) -> Parser<BufReader<'r>> {
        Parser::new_from_bytes_slice(source.as_bytes())
    }

    /// Convenience method to create a parser from a slice of bytes.
    #[inline]
    pub fn new_from_bytes_slice(source: &'r [u8]) -> Parser<BufReader<'r>> {
        Parser::new(BufReader::new(source))
    }
}

#[cfg(test)]
mod tests {
    use std::io::File;
    use std::io::buffered::BufferedReader;

    use super::{Parser, ParserConfig};

    fn test_sample(path: &str) {
        let file = File::open(&Path::new(path));
        let reader = BufferedReader::new(file);

        let mut parser = Parser::new_with_config(
            reader, ParserConfig::new()
                .ignore_comments(true)
                .whitespace_to_characters(true)
                .cdata_to_characters(true)
                .trim_whitespace(true)
                .coalesce_characters(true)
        );

        for e in parser.events() {
            println!("{}", e);
        }
    }

    #[test]
    fn sample_1_test() {
        test_sample("data/sample_1.xml");
    }

    #[test]
    fn sample_2_test() {
        test_sample("data/sample_2.xml");
    }

    #[test]
    fn sample_3_test() {
        test_sample("data/sample_3.xml");
    }

    #[test]
    fn sample_4_test() {
        test_sample("data/sample_4.xml");
    }
}
