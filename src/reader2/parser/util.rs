use std::ops::{Range, RangeFrom};
use std::io::Read;

#[cfg(feature = "encodings")]
use reader2::encodings::DelimitingReader;
use reader2::error::{Result, ParseError};

// never returns empty slice
pub fn read_up_to<'buf, R>(source: &mut DelimitingReader<R>,
                           buffer: &'buf mut String,
                           n: usize) -> Result<RangeFrom<usize>>
    where R: Read,
{
    let len_before = buffer.len();
    source.read_exact_chars(n, buffer)?;
    if len_before == buffer.len() {
        Err(ParseError::unexpected_eof_no_expected().into())
    } else {
        Ok(len_before..)
    }
}

// never returns empty slice
pub fn read_exact<R>(source: &mut DelimitingReader<R>,
                     buffer: &mut String,
                     n: usize) -> Result<RangeFrom<usize>>
    where R: Read,
{
    if source.read_exact_chars(n, buffer)? {
        Ok(buffer.len() - n..)
    } else {
        Err(ParseError::unexpected_eof_no_expected().into())
    }
}

// Reads from the source until encountered one of `chars`
// Returns a slice of the buffer corresponding to what was read excluding the matching char
pub fn read_until<R>(source: &mut DelimitingReader<R>,
                     buffer: &mut String,
                     chars: &[char]) -> Result<Range<usize>>
    where R: Read,
{
    let len_before = buffer.len();
    if source.read_until(chars, buffer)? {
        Ok(len_before..buffer.len()-1)
    } else {
        Err(ParseError::unexpected_eof(Some(chars)).into())
    }
}
