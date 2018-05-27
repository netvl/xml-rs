use std::ops::Range;
use std::io::Read;

use reader2::{DelimitingReader, Buffer};
use reader2::error::{Result, ParseError};

// never returns empty slice
pub fn read_up_to<'buf, R>(source: &mut DelimitingReader<R>,
                           buffer: &'buf mut Buffer,
                           n: usize) -> Result<Range<usize>>
    where R: Read,
{
    let Buffer { ref mut buf, ref mut len, } = buffer;
    // [..........]
    //      ^    ^
    //      |    |
    //     len  buf.len()

    let diff = buf.len() - *len;
    if n <= diff {
        *len += n;
        Ok(*len - n..*len)

    } else {
        //      - diff- n-diff -
        //      |     |        |
        // [..........]..........
        //      ^    ^         ^
        //      |    |         |
        //     len  buf.len() len+n
        
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
pub fn read_exact<R>(source: &mut DelimitingReader<R>,
                     buffer: &mut Buffer,
                     n: usize) -> Result<Range<usize>>
    where R: Read,
{
    let Buffer { ref mut buf, ref mut len } = buffer;
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
pub fn read_until<R>(source: &mut DelimitingReader<R>,
                     buffer: &mut Buffer,
                     chars: &[char]) -> Result<Range<usize>>
    where R: Read,
{
    let len_before = buffer.buf.len();
    if source.read_until(chars, &mut buffer.buf)? {
        buffer.len = buffer.buf.len();
        Ok(len_before..buffer.buf.len()-1)
    } else {
        Err(ParseError::unexpected_eof(Some(chars)).into())
    }
}
