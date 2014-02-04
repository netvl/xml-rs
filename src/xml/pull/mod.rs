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

pub struct Parser<B> {
    priv source: B,
    priv parser: PullParser
}

impl<B: Buffer> Parser<B> {
    #[inline]
    pub fn new(source: B) -> Parser<B> {
        Parser::new_with_config(source, ParserConfig::new())
    }

    #[inline]
    pub fn new_with_config(source: B, config: ParserConfig) -> Parser<B> {
        Parser { source: source, parser: parser::new(config) }
    }

    #[inline]
    pub fn next(&mut self) -> XmlEvent { 
        self.parser.next(&mut self.source)
    }

    #[inline]
    pub fn events<'a>(&'a mut self) -> Events<'a, B> {
        Events { parser: self, finished: false }
    }
}

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
