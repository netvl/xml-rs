use std::io::{self, Read};

use encoding_rs::{Decoder, DecoderResult, Encoding, UTF_8};

pub trait CharMatcher {
    fn matches(&mut self, c: char) -> bool;
}

impl CharMatcher for char {
    fn matches(&mut self, c: char) -> bool {
        *self == c
    }
}

impl<'a> CharMatcher for &'a [char] {
    fn matches(&mut self, c: char) -> bool { self.contains(&c) }
}

impl<F> CharMatcher for F where F: FnMut(char) -> bool {
    fn matches(&mut self, c: char) -> bool {
        (*self)(c)
    }
}

pub struct DelimitingReader<R: Read> {
    inner: DecodingReader<R>,
    buf: Box<str>,
    pos: usize,
    cap: usize,
}

impl<R: Read> DelimitingReader<R> {
    pub fn wrap(inner: DecodingReader<R>, buf_size: usize) -> Self {
        assert!(buf_size >= 4, "Buffer must contain space for at least one code point (4 bytes)");
        DelimitingReader {
            inner,
            buf: unsafe { String::from_utf8_unchecked(vec![0; buf_size]) }.into_boxed_str(),
            pos: 0,
            cap: 0,
        }
    }

    pub fn new_with_encoding(inner: R, encoding: &'static Encoding, decoding_buf_size: usize, buf_size: usize) -> Self {
        DelimitingReader::wrap(
            DecodingReader::new(inner, encoding, decoding_buf_size),
            buf_size
        )
    }

    pub fn new(inner: R, buf_size: usize) -> Self {
        DelimitingReader::new_with_encoding(inner, UTF_8, buf_size, buf_size)
    }

    pub fn read_exact_chars(&mut self, mut n: usize, target: &mut String) -> io::Result<bool> {
        while n > 0 {
            if self.pos == self.cap {
                if !self.decode_more()? {
                    return Ok(false);
                }
            }

            let actual_buf = &self.buf[self.pos..self.cap];

            let mut count = 0;
            let mut pos = 0;
            let mut c_len = 0;
            for (cur_pos, cur_c) in actual_buf.char_indices().take(n) {
                count += 1;
                pos = cur_pos;
                c_len = cur_c.len_utf8();
            }

            if count == n {
                let after_matching = pos + c_len;
                target.push_str(&actual_buf[..after_matching]);
                self.pos += after_matching;
                return Ok(true);
            } else {
                target.push_str(actual_buf);
                self.pos = self.cap;
                n -= count;
            }
        }
        Ok(true)
    }

    // Some(true) => separator found
    // Some(false) => EOF encountered, separator not found
    pub fn read_until<M>(&mut self, mut m: M, target: &mut String) -> io::Result<bool>
        where M: CharMatcher
    {
        loop {
            if self.pos == self.cap {
                if !self.decode_more()? {
                    return Ok(false);
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
                    target.push_str(actual_buf);
                    self.pos = self.cap;
                }
            }
        }
    }

    fn decode_more(&mut self) -> io::Result<bool> {
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
        Ok(true)
    }
}

pub struct DecodingReader<R: Read> {
    inner: R,
    decoder: Decoder,
    buf: Box<[u8]>,
    pos: usize,
    cap: usize,
    last_part_decoded: bool,
}

impl<R: Read> DecodingReader<R> {
    pub fn new(inner: R, encoding: &'static Encoding, buf_size: usize) -> Self {
        assert!(buf_size > 0, "Buffer cannot be empty");
        DecodingReader {
            inner,
            decoder: encoding.new_decoder_with_bom_removal(),
            buf: vec![0; buf_size].into_boxed_slice(),
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
    use std::io::{BufReader, Read};

    use encoding_rs::{UTF_8, UTF_16LE, UTF_16BE};
    use encoding::{Encoding, EncoderTrap};
    use encoding::all::UTF_16LE as UTF16_LE_ENC;
    use quickcheck::{quickcheck, TestResult};

    use super::*;

    #[test]
    fn test_read_until_simple_utf8() {
        let data = "şŏмę ŧĕ×ŧ - şёράŕẳť℮đ - wìŧĥ - ďåšћёš";
        let mut reader = DelimitingReader::new_with_encoding(data.as_bytes(), UTF_8, 16, 24);

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

        let mut reader = DelimitingReader::new_with_encoding(data, UTF_16BE, 16, 24);

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
    fn test_read_exact_chars_simple_utf8() {
        let data = "some данные";

        let mut reader = DelimitingReader::new_with_encoding(data.as_bytes(), UTF_8, 16, 16);

        let mut result = String::new();

        assert_eq!(reader.read_exact_chars(3, &mut result).unwrap(), true);
        assert_eq!(result, "som");
        result.clear();

        assert_eq!(reader.read_exact_chars(3, &mut result).unwrap(), true);
        assert_eq!(result, "e д");
        result.clear();

        assert_eq!(reader.read_exact_chars(4, &mut result).unwrap(), true);
        assert_eq!(result, "анны");
        result.clear();

        assert_eq!(reader.read_exact_chars(1, &mut result).unwrap(), true);
        assert_eq!(result, "е");
        result.clear();

        assert_eq!(reader.read_exact_chars(3, &mut result).unwrap(), false);
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

            let mut reader = DelimitingReader::new_with_encoding(
                source_data.as_bytes(),
                UTF_8,
                decoding_buf_cap,
                delim_buf_cap,
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

            let mut reader = DelimitingReader::new_with_encoding(
                &source_data[..],
                UTF_16LE,
                decoding_buf_cap,
                delim_buf_cap
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
