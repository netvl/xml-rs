use std::io::Read;
use std::ops::Range;

use reader2::{DelimitingReader, Buffer};
use reader2::error::{Result, ParseError};
use event::XmlEvent;
use chars::is_whitespace_char;

use super::Parser;
use super::util::*;

impl<R: Read> Parser<R> {
    pub(super) fn parse_doctype<'buf>(&mut self, buffer: &'buf mut Buffer) -> Result<XmlEvent<'buf>> {
        // At this point: buffer == '[whitespace]<!D'
        let r = read_exact(&mut self.source, buffer, 6)?;
        if &buffer[r.clone()] != "OCTYPE" {
            return Err(ParseError::unexpected_token(&buffer[r.start - 1..r.end], &["DOCTYPE"]).into());
        }

        let r2 = read_until_bracket_with_nesting(&mut self.source, buffer)?;
        let first_char = buffer[r2.clone()].chars().next().unwrap();
        if !is_whitespace_char(first_char) {
            return Err(ParseError::unexpected_token(
                &buffer[r.start - 1..r.end + first_char.len_utf8()],
                &["DOCTYPE followed by whitespace"]
            ).into());
        }

        let r = match buffer[r2.clone()].find(|c| !is_whitespace_char(c)) {
            Some(idx) => r2.start+idx..r2.end,
            None => r2.end..r2.end,
        };

        Ok(XmlEvent::DoctypeDeclaration {
            content: (&buffer[r]).into(),
        })
    }
}

pub fn read_until_bracket_with_nesting<R>(source: &mut DelimitingReader<R>,
                                          buffer: &mut Buffer) -> Result<Range<usize>>
    where R: Read
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
