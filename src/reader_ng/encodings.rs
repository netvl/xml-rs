use std::io::{self, BufRead};
use std::str;

use encoding_rs::{Decoder, DecoderResult, Encoding};

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

pub struct BufReaderWithEncoding<R: BufRead> {
    inner: R,
    decoder: Decoder,
    buf: Box<[u8]>,
    pos: usize,
    cap: usize,
    last_part_decoded: bool,
}

impl<R: BufRead> BufReaderWithEncoding<R> {
    pub fn new(reader: R,
               encoding: &'static Encoding,
               size: usize) -> BufReaderWithEncoding<R> {
        assert!(size >= 4, "Buffer size cannot be less than one code point size (4 bytes)");

        BufReaderWithEncoding {
            inner: reader,
            decoder: encoding.new_decoder_with_bom_removal(),
            buf: vec![0; size].into_boxed_slice(),
            pos: 0,
            cap: 0,
            last_part_decoded: false,
        }
    }

    pub fn read_until<M>(&mut self, mut m: M, target: &mut String) -> io::Result<bool>
        where M: CharMatcher
    {
        loop {
            // ensure we have some data in the buffer
            self.fill_decoded_buf()?;

            if self.pos == self.cap {
                // this means that the underlying reader is at its end, so we exit as well
                return Ok(false);
            }

            // it is guaranteed that &buf[pos..cap] is a valid UTF-8 string
            let buf = unsafe { str::from_utf8_unchecked(&self.buf[self.pos..self.cap]) };
            match buf.char_indices().find(|&(pos, c)| m.matches(c)) {
                // found matching character, push everything up to and including it
                // to output and return
                Some((pos, c)) => {
                    let after_matching = pos + c.len_utf8();
                    target.push_str(&buf[self.pos..self.pos + after_matching]);
                    self.pos += after_matching;
                    return Ok(true);
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

        let (result, bytes_read, bytes_written) = {
            let buf = self.inner.fill_buf()?;

            // Reached end-of-file
            if buf.is_empty() {
                if self.last_part_decoded {
                    // Everything is processed
                    return Ok(());
                } else {
                    // Need to call the decoder with last=true
                    let (result, bytes_read, bytes_written) =
                        self.decoder.decode_to_utf8_without_replacement(buf, &mut self.buf, true);
                    self.last_part_decoded = true;

                    match result {
                        // The entire stream has been processed
                        DecoderResult::InputEmpty => return Ok(()),
                        // Cannot happen - buffer always has enough space
                        DecoderResult::OutputFull => unreachable!(),
                        // Invalid data encountered
                        DecoderResult::Malformed(_, _) =>
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                "Input stream contains byte sequence which is invalid for the configured encoding",
                            )),
                    }
                }
            }


            self.decoder.decode_to_utf8_without_replacement(buf, &mut self.buf, false)
        };

        self.pos = 0;
        self.cap = bytes_written;
        self.inner.consume(bytes_read);

        match result {
            // In both these cases we need to decode more data, which is postponed until
            // the decoded buffer is exhausted
            DecoderResult::OutputFull | DecoderResult::InputEmpty => {
                Ok(())
            }
            // This error is recoverable - just reading again is fine
            DecoderResult::Malformed(_, _) => {
                Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Input stream contains byte sequence which is invalid for the configured encoding",
                ))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use encoding_rs::UTF_8;
    use std::io::{BufRead, BufReader, Read};
    use super::*;

    #[test]
    fn test_read_until_simple_utf8() {
        let data = "şŏмę ŧĕ×ŧ - şёράŕẳť℮đ - wìŧĥ - ďåšћёš -";
        let mut reader = BufReaderWithEncoding::new(BufReader::with_capacity(16, data.as_bytes()), UTF_8, 24);

        let mut result = String::new();

        assert_eq!(reader.read_until('-', &mut result).unwrap(), true);
        assert_eq!(result, "şŏмę ŧĕ×ŧ -");
        result.clear();

        assert_eq!(reader.read_until('-', &mut result).unwrap(), true);
        assert_eq!(result, " şёράŕẳť℮đ -");
        result.clear();

        assert_eq!(reader.read_until('-', &mut result).unwrap(), true);
        assert_eq!(result, " wìŧĥ -");
        result.clear();

        assert_eq!(reader.read_until('-', &mut result).unwrap(), true);
        assert_eq!(result, " ďåšћёš -");
        result.clear();

        assert_eq!(reader.read_until('-', &mut result).unwrap(), false);
        assert!(result.is_empty());
        result.clear();
    }
}
