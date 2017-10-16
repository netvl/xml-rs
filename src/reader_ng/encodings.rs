use std::io::{self, BufRead};
use std::str;

use encoding_rs::{Encoding, Decoder, DecoderResult};

pub struct BufReaderWithEncoding<R: BufRead> {
    inner: BufRead,
    decoder: Decoder,
    buf: Box<[u8]>,
    pos: usize,
    cap: usize,
}

impl<R: BufRead> BufReaderWithEncoding<R> {
    pub fn new(reader: R,
               encoding: &'static Encoding,
               size: usize) -> BufReaderWithEncoding<R> {
        BufReaderWithEncoding {
            inner: reader,
            decoder: encoding.new_decoder_with_bom_removal(),
            buf: vec![0; size].into_boxed_slice(),
            pos: 0,
            cap: 0,
        }
    }

    pub fn read_until<M>(&mut self, mut m: M, target: &mut String) -> io::Result<()>
        where M: CharMatcher
    {
        loop {
            // ensure we have some data in the buffer
            self.fill_decoded_buf()?;

            if self.pos == self.cap {
                // this means that the underlying reader is at its end, so we exit as well
                return Ok(());
            }

            // it is guaranteed that &buf[pos..cap] is a valid UTF-8 string
            let buf = unsafe { str::from_utf8_unchecked(&self.buf[self.pos..self.cap]) };
            match buf.char_indices().find(|&(pos, c)| m.matches(c)) {
                // found matching character, push everything before it to output and return
                Some((pos, _)) => {
                    target.push_str(&buf[self.pos..pos]);
                    self.pos = pos;
                    return Ok(());
                }
                // character not found, push the entire buffer to output and try again
                None => {
                    target.push_str(buf);
                    self.pos = self.cap;
                }
            }
        }
    }

    fn fill_decoded_buf(&mut self) -> io::Result<()> {
        if self.pos < self.cap {
            // We still have some data left in the decoded buffer
            return Ok(());
        }

        // Now pos == cap, meaning the internal buffer is exhausted

        let buf = self.inner.fill_buf()?;

        if buf.is_empty() {
            // Underlying reader is empty
            return Ok(());
        }

        let (result, bytes_read, bytes_written) = self.decoder.decode_to_utf8(buf, &mut self.buf, false);

        match result {
            DecoderResult::OutputFull => {
                self.pos = 0;
                self.cap = bytes_written;
                self.inner.consume(bytes_read);
            }
        }
    }
}


trait CharMatcher {
    fn matches(&mut self, c: char) -> bool;
}

impl CharMatcher for char {
    fn matches(&mut self, c: char) -> bool {
        *self == c
    }
}

impl<F> CharMatcher for F where F: FnMut(char) -> bool {
    fn matches(&mut self, c: char) -> bool {
        (*self)(c)
    }
}
