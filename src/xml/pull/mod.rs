use std::io::Buffer;
use std::io::mem::{MemReader, BufReader};

use events::XmlEvent;
use self::parser::PullParser;

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

pub struct Parser<B> {
    priv source: B,
    priv parser: PullParser
}

impl<B: Buffer> Parser<B> {
    #[inline]
    pub fn new(source: B) -> Parser<B> {
        Parser { source: source, parser: parser::new() }
    }

    #[inline]
    pub fn next(&mut self) -> XmlEvent { 
        self.parser.next(&mut self.source)
    }
}

impl Parser<MemReader> {
    #[inline]
    pub fn new_from_str(source: ~str) -> Parser<MemReader> {
        Parser::new_from_bytes(source.into_bytes())
    }

    #[inline]
    pub fn new_from_bytes(source: ~[u8]) -> Parser<MemReader> {
        Parser::new(MemReader::new(source))
    }

}

impl<'r> Parser<BufReader<'r>> {
    #[inline]
    pub fn new_from_str_slice(source: &'r str) -> Parser<BufReader<'r>> {
        Parser::new_from_bytes_slice(source.as_bytes())
    }

    #[inline]
    pub fn new_from_bytes_slice(source: &'r [u8]) -> Parser<BufReader<'r>> {
        Parser::new(BufReader::new(source))
    }
}
