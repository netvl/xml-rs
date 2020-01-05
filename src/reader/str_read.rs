use std::io;
use std::io::BufRead;

use crate::reader::decoding_reader::DecodingReader;

pub trait StrRead {
    /// Reads a UTF-8 piece of data into the provided buffer.
    ///
    /// Returns a boolean value indicating whether there is more data to read. In other words, if
    /// this method returns `false`, then the reader has reached the end of the stream.
    fn read_str_data(&mut self, dst: &mut String) -> io::Result<bool>;
}

impl<R: BufRead> StrRead for DecodingReader<R> {
    fn read_str_data(&mut self, dst: &mut String) -> io::Result<bool> {
        self.decode_to_string(dst)
    }
}

impl<R: BufRead> StrRead for R {
    fn read_str_data(&mut self, dst: &mut String) -> io::Result<bool> {
        todo!()
    }
}
