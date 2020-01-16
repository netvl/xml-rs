use std::io;
use std::io::{BufRead, Error};

use crate::reader::decoding_reader::DecodingReader;

pub trait StrRead {
    /// Checks whether the target buffer will be reallocated if `read_str_data` is called on it now.
    fn need_to_grow(&mut self, dst: &String) -> io::Result<bool>;

    /// Reads a UTF-8 piece of data into the provided buffer.
    ///
    /// Returns a boolean value indicating whether there is more data to read. In other words, if
    /// this method returns `false`, then the reader has reached the end of the stream.
    fn read_str_data(&mut self, dst: &mut String) -> io::Result<bool>;
}

impl<R: BufRead> StrRead for DecodingReader<R> {
    fn need_to_grow(&mut self, dst: &String) -> Result<bool, Error> {
        self.will_grow(dst)
    }

    fn read_str_data(&mut self, dst: &mut String) -> io::Result<bool> {
        self.decode_to_string(dst)
    }
}

impl<R: BufRead> StrRead for R {
    fn need_to_grow(&mut self, dst: &String) -> Result<bool, Error> {
        todo!()
    }

    fn read_str_data(&mut self, dst: &mut String) -> io::Result<bool> {
        todo!()
    }
}
