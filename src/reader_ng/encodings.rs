use std::io::{self, Read};
use std::ops::{Deref, DerefMut};
use std::str;

use encoding_rs::{Decoder, DecoderResult, Encoding};

use super::buffer::{StrBuffer, Buffer};

pub trait CharMatcher {
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

pub struct DelimitingReader<'dbuf, 'buf, R: Read> {
    inner: DecodingReader<'dbuf, R>,
    buf: StrBuffer<'buf>,
    pos: usize,
    cap: usize,
}

impl<'dbuf, 'buf, R: Read> DelimitingReader<'dbuf, 'buf, R> {
    pub fn wrap(inner: DecodingReader<'dbuf, R>, buf: StrBuffer<'buf>) -> Self {
        assert!(buf.len() >= 4, "Buffer must contain space for at least one code point (4 bytes)");
        DelimitingReader {
            inner,
            buf,
            pos: 0,
            cap: 0,
        }
    }

    pub fn new(inner: R, encoding: &'static Encoding, decoding_buf: Buffer<'dbuf>, buf: StrBuffer<'buf>) -> Self {
        DelimitingReader::wrap(
            DecodingReader::new(inner, encoding, decoding_buf),
            buf
        )
    }

    // Some(true) => separator found
    // Some(false) => EOF encountered, separator not found
    pub fn read_until<M>(&mut self, mut m: M, target: &mut String) -> io::Result<bool>
        where M: CharMatcher
    {
        loop {
            if self.pos == self.cap {
                loop {
                    match self.inner.decode_to_str(&mut self.buf)? {
                        // EOF
                        None => return Ok(false),
                        // this can happen if underlying decoding buffer is too small to accomodate
                        // one code point of the underlying encoding, which would require multiple
                        // read operations to decode one code point
                        Some(0) => continue,
                        Some(bytes_read) => {
                            self.pos = 0;
                            self.cap = bytes_read;
                            break;
                        }
                    }
                }
            }

            let actual_buf = &self.buf[self.pos..self.cap];
            match actual_buf.char_indices().find(|&(pos, c)| m.matches(c)) {
                // found matching character, push everything up to and including it
                // to output and return
                Some((pos, c)) => {
                    let after_matching = pos + c.len_utf8();
                    target.push_str(&actual_buf[..after_matching]);
                    self.pos += after_matching;
                    return Ok(true);
                }
                // character not found, push the entire buffer to output and try again
                None => {
                    target.push_str(&actual_buf);
                    self.pos = self.cap;
                }
            }
        }
    }
}

pub struct DecodingReader<'buf, R: Read> {
    inner: R,
    decoder: Decoder,
    buf: Buffer<'buf>,
    pos: usize,
    cap: usize,
    last_part_decoded: bool,
}

impl<'buf, R: Read> DecodingReader<'buf, R> {
    pub fn new(inner: R, encoding: &'static Encoding, buf: Buffer<'buf>) -> Self {
        assert!(buf.len() > 0, "Buffer cannot be empty");
        DecodingReader {
            inner,
            decoder: encoding.new_decoder_with_bom_removal(),
            buf,
            pos: 0,
            cap: 0,
            last_part_decoded: false,
        }
    }

    // None => encountered EOF
    // Some(0) => nothing was written to dst
    //            can happen when decoding one code point; need to call this method again
    // Some(n) => n bytes were written to dst
    pub fn decode_to_str(&mut self, dst: &mut str) -> io::Result<Option<usize>> {
        if self.pos == self.cap {
            let bytes_read;
            loop {
                match self.inner.read(&mut self.buf) {
                    Ok(n) => {
                        bytes_read = n;
                        break;
                    }
                    Err(ref e) if e.kind() == io::ErrorKind::Interrupted => continue,
                    Err(e) => return Err(e),
                }
            }

            // EOF
            if bytes_read == 0 {
                return self.handle_eof_str(dst);
            }

            self.cap = bytes_read;
            self.pos = 0;
        }

        let remaining_buf = &self.buf[self.pos..self.cap];

        let (result, bytes_read, bytes_written) = self.decoder.decode_to_str_without_replacement(remaining_buf, dst, false);
        self.pos += bytes_read;

        match result {
            DecoderResult::InputEmpty | DecoderResult::OutputFull => Ok(Some(bytes_written)),
            DecoderResult::Malformed(_, _) => {
                Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Input stream contains byte sequence which is invalid for the configured encoding",
                ))
            }
        }
    }

    fn handle_eof_str(&mut self, dst: &mut str) -> io::Result<Option<usize>> {
        if self.last_part_decoded {
            Ok(None)
        } else {
            let (result, bytes_read, bytes_written) = self.decoder.decode_to_str_without_replacement(&[], dst, true);

            match result {
                DecoderResult::InputEmpty => {
                    self.last_part_decoded = true;
                    Ok(Some(bytes_written))
                }
                DecoderResult::OutputFull => Ok(Some(bytes_written)),
                DecoderResult::Malformed(_, _) => {
                    Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Input stream contains byte sequence which is invalid for the configured encoding",
                    ))
                }
            }
        }
    }
}
#[cfg(test)]
mod tests {
    use std::io::{BufRead, BufReader, Read};

    use encoding_rs::{UTF_8, UTF_16LE, UTF_16BE};
    use encoding::{Encoding, EncoderTrap};
    use encoding::all::UTF_16LE as UTF16_LE_ENC;
    use quickcheck::{quickcheck, TestResult};

    use super::*;
    use super::super::buffer::{Buffer, StrBuffer};

    #[test]
    fn test_read_until_simple_utf8() {
        let data = "şŏмę ŧĕ×ŧ - şёράŕẳť℮đ - wìŧĥ - ďåšћёš";
        let mut reader = DelimitingReader::new(
            data.as_bytes(),
            UTF_8,
            Buffer::new_owned(16),
            StrBuffer::new_owned(24)
        );

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

        assert_eq!(reader.read_until('-', &mut result).unwrap(), false);
        assert_eq!(result, " ďåšћёš");
        result.clear();

        assert_eq!(reader.read_until('-', &mut result).unwrap(), false);
        assert!(result.is_empty());
    }

    #[test]
    fn test_read_until_simple_utf16() {
        // "şŏмę ŧĕ×ŧ - şёράŕẳť℮đ - wìŧĥ - ďåšћёš" in UTF-16BE
        let data: &[u8] = &[
            0x01, 0x5f, 0x01, 0x4f, 0x04, 0x3c, 0x01, 0x19, 0x00, 0x20, 0x01, 0x67,
            0x01, 0x15, 0x00, 0xd7, 0x01, 0x67, 0x00, 0x20, 0x00, 0x2d, 0x00, 0x20,
            0x01, 0x5f, 0x04, 0x51, 0x03, 0xc1, 0x03, 0xac, 0x01, 0x55, 0x1e, 0xb3,
            0x01, 0x65, 0x21, 0x2e, 0x01, 0x11, 0x00, 0x20, 0x00, 0x2d, 0x00, 0x20,
            0x00, 0x77, 0x00, 0xec, 0x01, 0x67, 0x01, 0x25, 0x00, 0x20, 0x00, 0x2d,
            0x00, 0x20, 0x01, 0x0f, 0x00, 0xe5, 0x01, 0x61, 0x04, 0x5b, 0x04, 0x51,
            0x01, 0x61,
        ];

        let mut reader = DelimitingReader::new(
            data,
            UTF_16BE,
            Buffer::new_owned(16),
            StrBuffer::new_owned(24)
        );

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

        assert_eq!(reader.read_until('-', &mut result).unwrap(), false);
        assert_eq!(result, " ďåšћёš");
        result.clear();

        assert_eq!(reader.read_until('-', &mut result).unwrap(), false);
        assert!(result.is_empty());
    }

    #[test]
    fn test_read_until_utf8_buffer_sizes() {
        fn prop(decoding_buf_cap: usize, delim_buf_cap: usize, parts: Vec<String>) -> TestResult {
            if decoding_buf_cap > 2048 || delim_buf_cap > 2048 || delim_buf_cap < 4 || decoding_buf_cap == 0 {
                return TestResult::discard();
            }

            if parts.iter().any(|s| s.contains('-')) {
                return TestResult::discard();
            }

            let source_data = parts.join("-");

            let mut reader = DelimitingReader::new(
                source_data.as_bytes(),
                UTF_8,
                Buffer::new_owned(decoding_buf_cap),
                StrBuffer::new_owned(delim_buf_cap),
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

    #[test]
    fn test_read_until_utf16_buffer_sizes() {
        fn prop(decoding_buf_cap: usize, delim_buf_cap: usize, parts: Vec<String>) -> TestResult {
            if decoding_buf_cap > 2048 || delim_buf_cap > 2048 || delim_buf_cap < 4 || decoding_buf_cap == 0 {
                return TestResult::discard();
            }

            if parts.iter().any(|s| s.contains('-')) {
                return TestResult::discard();
            }

            let source_data_utf8 = parts.join("-");
            let source_data = UTF16_LE_ENC.encode(&source_data_utf8, EncoderTrap::Ignore).unwrap();

            let mut reader = DelimitingReader::new(
                &source_data[..],
                UTF_16LE,
                Buffer::new_owned(decoding_buf_cap),
                StrBuffer::new_owned(delim_buf_cap),
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

            if result != source_data_utf8 {
                return TestResult::error(format!("Invalid final result: {:?}, expected: {:?}", result, source_data));
            }

            TestResult::passed()
        }
        quickcheck(prop as fn(usize, usize, Vec<String>) -> TestResult);
    }
}
