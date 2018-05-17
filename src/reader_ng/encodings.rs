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

pub struct EncodingReader<R: BufRead> {
    inner: R,
    decoder: Decoder,
    buf: Box<[u8]>,
    pos: usize,
    cap: usize,
    last_part_decoded: bool,
}

impl<R: BufRead> EncodingReader<R> {
    pub fn new(reader: R,
               encoding: &'static Encoding,
               size: usize) -> EncodingReader<R> {
        assert!(size >= 4, "Buffer size cannot be less than one code point size (4 bytes)");

        EncodingReader {
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
                println!("Position still == capacity, EOF");
                // this means that the underlying reader is at its end, so we exit as well
                return Ok(false);
            }

            // it is guaranteed that &buf[pos..cap] is a valid UTF-8 string
            let buf = unsafe { str::from_utf8_unchecked(&self.buf[self.pos..self.cap]) };
            println!("Looking for the next separator in {} bytes of the buffer", buf.len());
            match buf.char_indices().find(|&(pos, c)| m.matches(c)) {
                // found matching character, push everything up to and including it
                // to output and return
                Some((pos, c)) => {
                    println!("Found separator {} at {}", c, pos);
                    let after_matching = pos + c.len_utf8();
                    target.push_str(&buf[self.pos..self.pos + after_matching]);
                    self.pos += after_matching;
                    return Ok(true);
                }
                // character not found, push the entire buffer to output and try again
                None => {
                    println!("Separator not found, attempting again");
                    target.push_str(buf);
                    self.pos = self.cap;
                }
            }
        }
    }

    // returns true if capacity has been updated, false otherwise
    fn fill_decoded_buf(&mut self) -> io::Result<bool> {
        println!("Filling decoded buffer");

        if self.pos < self.cap {
            println!("Position < capacity, still have some data");
            // We still have some data left in the decoded buffer
            return Ok(false);
        }

        println!("Position == capacity, decoded buffer is exhausted");

        // Now pos == cap, meaning the internal buffer is exhausted
        let old_cap = self.cap;

        let (result, bytes_read, bytes_written) = {
            let buf = self.inner.fill_buf()?;
            println!("Filled bufreader buffer: {}", buf.len());

            // Reached end-of-file
            if buf.is_empty() {
                println!("Bufreader buffer is empty, EOF");
                if self.last_part_decoded {
                    println!("Last part has already been decoded");
                    // Everything is processed
                    return Ok(false);
                } else {
                    println!("Last part has not been decoded yet, finalizing");
                    // Need to call the decoder with last=true
                    let (result, bytes_read, bytes_written) =
                        self.decoder.decode_to_utf8_without_replacement(buf, &mut self.buf, true);
                    self.last_part_decoded = true;

                    println!("Decoding result: read={}, written={}, result={:?}", bytes_read, bytes_written, result);

                    self.pos = 0;
                    self.cap = bytes_written;
                    println!("New capacity: {}, new position: {}", self.cap, self.pos);

                    match result {
                        // The entire stream has been processed
                        DecoderResult::InputEmpty => return Ok(self.cap != old_cap),
                        // Cannot happen at this point - buffer always has enough space
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

            println!("Decoding bufread buffer");
            self.decoder.decode_to_utf8_without_replacement(buf, &mut self.buf, false)
        };

        println!("Decoding result: read={}, written={}, result={:?}", bytes_read, bytes_written, result);

        self.pos = 0;
        self.cap = bytes_written;
        self.inner.consume(bytes_read);
        println!("New capacity: {}, new position: {}", self.cap, self.pos);
        println!("Consumed {} bytes from the bufreader buffer", bytes_read);

        match result {
            // In both these cases we need to decode more data, which is postponed until
            // the decoded buffer is exhausted
            DecoderResult::OutputFull | DecoderResult::InputEmpty => {
                Ok(self.cap != old_cap)
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
    use std::io::{BufRead, BufReader, Read};

    use encoding_rs::UTF_8;
    use quickcheck::{TestResult, quickcheck};

    use super::*;

    #[test]
    fn test_read_until_simple_utf8() {
        let data = "şŏмę ŧĕ×ŧ - şёράŕẳť℮đ - wìŧĥ - ďåšћёš -";
        let mut reader = EncodingReader::new(BufReader::with_capacity(16, data.as_bytes()), UTF_8, 24);

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
    }

    #[test]
    fn test_read_until_utf8_small() {
        let data = "\u{80}";
        let mut reader = EncodingReader::new(BufReader::with_capacity(1, data.as_bytes()), UTF_8, 4);

        let mut result = String::new();

        assert_eq!(reader.read_until('-', &mut result).unwrap(), false);
        assert_eq!(result, data);
    }

    #[test]
    fn test_read_until_utf8_buffer_sizes() {
        fn prop(base_cap: usize, decoded_cap: usize, parts: Vec<String>) -> TestResult {
            if base_cap > 2048 || decoded_cap > 2048 || decoded_cap < 4 || base_cap == 0 {
                return TestResult::discard();
            }

            let source_data = parts.join("-");
            let mut reader = EncodingReader::new(
                BufReader::with_capacity(base_cap, source_data.as_bytes()),
                UTF_8,
                decoded_cap
            );

            let mut result = String::new();
            let mut i = 0;
            while reader.read_until('-', &mut result).unwrap() {
                i += 1;
                let expected = parts[..i].join("-") + "-";
                if result != expected {
                    return TestResult::error(
                        format!("Invalid intermediate result: {:?}, expected: {:?}", result, expected)
                    );
                }
            }

            if result != source_data {
                return TestResult::error(format!("Invalid final result: {:?}, expected: {:?}", result, source_data));
            }

            TestResult::passed()
        }
        quickcheck(prop as fn(usize, usize, Vec<String>) -> TestResult);
    }
}
