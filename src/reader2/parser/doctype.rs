use std::io::Read;

use crate::chars::is_whitespace_char;
use crate::event::XmlEvent;

use super::super::error::{ParseError, Result};
use super::super::Buffer;
use super::util::*;
use super::Parser;

impl<R: Read> Parser<R> {
    pub(super) fn parse_doctype<'buf>(&mut self, buffer: &'buf mut Buffer) -> Result<XmlEvent<'buf>> {
        // At this point: buffer == '[whitespace]<!D'
        debug_assert!(buffer.ends_with("<!D"));

        let r = read_exact(&mut self.source, buffer, 6)?;
        if &buffer[r.clone()] != "OCTYPE" {
            return Err(ParseError::unexpected_token(&buffer[r.start - 1..r.end], &["DOCTYPE"]).into());
        }

        let r2 = read_until_bracket_with_nesting(&mut self.source, buffer)?;
        // TODO: verify that r2 is indeed non-empty at this point
        let first_char = buffer[r2.clone()].chars().next().unwrap();
        if !is_whitespace_char(first_char) {
            return Err(ParseError::unexpected_token(
                &buffer[r.start - 1..r.end + first_char.len_utf8()],
                &["DOCTYPE followed by whitespace"],
            )
            .into());
        }

        let r = match buffer[r2.clone()].find(|c| !is_whitespace_char(c)) {
            Some(idx) => r2.start + idx..r2.end,
            None => r2.end..r2.end,
        };

        Ok(XmlEvent::doctype_declaration(&buffer[r]))
    }
}
