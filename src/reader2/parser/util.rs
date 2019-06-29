use std::io::Read;
use std::ops::Range;

use super::super::error::{ParseError, Result};
use super::super::{Buffer, DelimitingReader};

// never returns empty slice
pub fn read_up_to<R>(source: &mut DelimitingReader<R>, buffer: &mut Buffer, n: usize) -> Result<Range<usize>>
where
    R: Read,
{
    let Buffer {
        ref mut buf,
        ref mut len,
    } = buffer;
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

        source.read_exact_chars(n - diff, buf)?;

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
pub fn read_exact<R>(source: &mut DelimitingReader<R>, buffer: &mut Buffer, n: usize) -> Result<Range<usize>>
where
    R: Read,
{
    let Buffer {
        ref mut buf,
        ref mut len,
    } = buffer;
    // [..........]
    //      ^    ^
    //      |    |
    //     len  buf.len()

    let diff = buf.len() - *len;
    if n <= diff {
        *len += n;
        Ok(*len - n..*len)
    } else {
        if source.read_exact_chars(n - diff, buf)? {
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
pub fn read_until<R>(source: &mut DelimitingReader<R>, buffer: &mut Buffer, chars: &[char]) -> Result<Range<usize>>
where
    R: Read,
{
    let Buffer {
        ref mut buf,
        ref mut len,
    } = buffer;
    // [..........]
    //      ^    ^
    //      |    |
    //     len  buf.len()

    if *len < buf.len() {
        match buf[*len..].char_indices().find(|&(_, c)| chars.contains(&c)) {
            Some((pos, c)) => {
                *len = *len + pos;
                return Ok(*len - pos..*len - 1);
            }
            None => *len = buf.len(),
        }
    }

    // now len == buf.len()

    let len_before = buf.len();
    if source.read_until(chars, buf)? {
        *len = buf.len();
        Ok(len_before..buf.len() - 1)
    } else {
        Err(ParseError::unexpected_eof(Some(chars)).into())
    }
}

pub fn read_until_bracket_with_nesting<R>(source: &mut DelimitingReader<R>, buffer: &mut Buffer) -> Result<Range<usize>>
where
    R: Read,
{
    let mut depth = 1;
    let mut range = buffer.len()..buffer.len();
    while depth > 0 {
        let r = read_until(source, buffer, &['<', '>'])?;
        match buffer.last() {
            '<' => depth += 1,
            '>' => depth -= 1,
            _ => unreachable!(),
        }
        range = range.start..r.end;
    }
    Ok(range)
}
