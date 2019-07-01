use std::io;
use std::io::{Read, BufRead};

use self::parser::Parser;
use self::decoding_reader::DecodingReader;

pub mod decoding_reader;
pub mod error;
pub mod parser;

pub trait StrRead {
    /// Reads a UTF-8 piece of data into the provided buffer.
    ///
    /// Returns the number of the bytes read. It is guaranteed that `dst[..bytes_read]` is a valid
    /// UTF-8 string. If `0` is returned, it means one of the two things:
    ///
    /// 1. This reader has reached the end of file and will not produce more data.
    /// 2. The specified `dst` buffer has length 0.
    fn read_str_data(&mut self, dst: &mut String) -> io::Result<bool>;
}

impl<R: BufRead> StrRead for DecodingReader<R> {
    fn read_str_data(&mut self, dst: &mut String) -> io::Result<bool> {
        self.decode_to_string(dst)
    }
}

pub fn new_parser<R: BufRead>(source: R) -> Parser<DecodingReader<R>> {
    Parser::new(DecodingReader::new(source, encoding_rs::UTF_8))
}
