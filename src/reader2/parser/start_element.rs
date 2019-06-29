use std::io::Read;

use super::super::error::{ParseError, Result};
use super::super::Buffer;
use super::attributes::Attributes;
use super::util::*;
use super::Parser;
use crate::chars::{is_name_char, is_name_start_char};
use crate::event::XmlEvent;
use crate::name2::Name;

impl<R: Read> Parser<R> {
    pub(crate) fn parse_start_element<'buf>(&mut self, buffer: &'buf mut Buffer) -> Result<XmlEvent<'buf>> {
        // At this point: buffer == '[whitespace]<[x]'
        let name_r = match buffer.last() {
            '<' => {
                read_exact(&mut self.source, buffer, 1)?;
                return self.parse_start_element(buffer);
            }
            c if is_name_start_char(c) => {
                let r = read_until(&mut self.source, buffer, chars!(WHITESPACE, '/', '>'))?;

                if let Some(idx) = buffer[r.clone()].find(|c| !is_name_char(c)) {
                    return Err(ParseError::unexpected_token(buffer.at_idx(r.start + idx), &["name character"]).into());
                }

                (r.start - 1)..r.end
            }
            _ => {
                return Err(ParseError::unexpected_token(buffer.last_str(), &["name start character"]).into());
            }
        };

        let attributes = match buffer.last() {
            '/' | '>' => Vec::new(),
            _ => {
                let r = read_until(&mut self.source, buffer, &['>'])?;
                Attributes::parse(&buffer[r], '>')?
            }
        };
        let name = Name::from_str(&buffer[name_r]).unwrap(); // FIXME

        Ok(XmlEvent::start_element(name, attributes))
    }
}
