use std::io::Read;
use std::ops::{Deref, DerefMut, Range};

use super::DelimitingReader;
use super::error::{Result, ParseError};

pub struct Buffer {
    pub(super) buf: String,
    pub(super) len: usize,
}

impl Deref for Buffer {
    type Target = str;

    fn deref(&self) -> &str {
        &self.buf[..self.len]
    }
}

impl DerefMut for Buffer {
    fn deref_mut(&mut self) -> &mut str {
        &mut self.buf[..self.len]
    }
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            buf: String::new(),
            len: 0,
        }
    }

    pub fn from<'a, 'b, R: Read>(&'a mut self, r: &'b mut DelimitingReader<R>) -> BufferAndReader<'a, 'b, R> {
        BufferAndReader {
            b: self,
            r,
        }
    }

    pub fn clear(&mut self) {
        self.len = 0;
        self.buf.clear();
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn at_idx(&self, idx: usize) -> char { self[idx..].chars().next().unwrap() }

    pub fn first(&self) -> char {
        self[..].chars().next().unwrap()
    }

    pub fn last(&self) -> char {
        self[..].chars().rev().next().unwrap()
    }

    pub fn last_str(&self) -> &str {
        let (idx, _) = self[..].char_indices().rev().next().unwrap();
        &self[idx..]
    }
}

pub struct BufferAndReader<'a, 'b, R> {
    pub b: &'a mut Buffer,
    pub r: &'b mut DelimitingReader<R>,
}

impl<'a, 'b, R: Read> BufferAndReader<'a, 'b, R> {
    // never returns empty slice
    pub fn read_up_to(&mut self, n: usize) -> Result<Range<usize>> {
        let Buffer { ref mut buf, ref mut len, } = self.b;
        // [..........]
        //      ^    ^
        //      |    |
        //     len  buf.len()

        let diff = buf.len() - *len;
        if n <= diff {
            *len += n;
            Ok(*len - n..*len)

        } else {
            //      - diff - n-diff -
            //      |      |        |
            // [...........]..........
            //      ^     ^         ^
            //      |     |         |
            //     len   buf.len() len+n

            self.r.read_exact_chars(n - diff, buf)?;

            if *len == buf.len() {
                // nothing was read, and diff is zero so nothing to return
                Err(ParseError::unexpected_eof_no_expected().into())
            } else {
                let r = *len..buf.len();
                *len = buf.len();
                Ok(r)
            }
        }
    }

    // never returns empty slice
    pub fn read_exact(&mut self, n: usize) -> Result<Range<usize>> {
        let Buffer { ref mut buf, ref mut len } = self.b;
        // [..........]
        //      ^    ^
        //      |    |
        //     len  buf.len()

        let diff = buf.len() - *len;
        if n <= diff {
            *len += n;
            Ok(*len - n..*len)
        } else {
            if self.r.read_exact_chars(n - diff, buf)? {
                let r = *len..buf.len();
                *len = buf.len();
                Ok(r)
            } else {
                Err(ParseError::unexpected_eof_no_expected().into())
            }
        }
    }

    // Reads from the source until encountered one of `chars`
    // Returns a slice of the buffer corresponding to what was read excluding the matching char
    pub fn read_until(&mut self, chars: &[char]) -> Result<Range<usize>> {
        let Buffer { ref mut buf, ref mut len } = self.b;
        // [..........]
        //      ^    ^
        //      |    |
        //     len  buf.len()

        if *len < buf.len() {
            match buf[*len..].char_indices().find(|&(_, c)| chars.contains(&c)) {
                Some((pos, c)) => {
                    *len = *len + pos;
                    return Ok(*len - pos..*len - 1)
                }
                None => *len = buf.len(),
            }
        }

        // now len == buf.len()

        let len_before = buf.len();
        if self.r.read_until(chars, buf)? {
            *len = buf.len();
            Ok(len_before..buf.len()-1)
        } else {
            Err(ParseError::unexpected_eof(Some(chars)).into())
        }
    }

    pub fn read_until_bracket_with_nesting<R>(&mut self) -> Result<Range<usize>> {
        let mut depth = 1;
        let mut range = self.b.len()..self.b.len();
        while depth > 0 {
            let r = self.read_until(&['<', '>'])?;
            match self.b.last() {
                '<' => depth += 1,
                '>' => depth -= 1,
                _ => unreachable!(),
            }
            range = range.start..r.end;
        }
        Ok(range)
    }
}
