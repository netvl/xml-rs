use std::io::Read;

use reader2::Buffer;
use reader2::error::{Result, ParseError};
use event::XmlEvent;

use super::Parser;
use super::util::*;

impl<R: Read> Parser<R> {
    pub(super) fn parse_comment<'buf>(&mut self, buffer: &'buf mut Buffer) -> Result<XmlEvent<'buf>> {
        // At this point: buffer == '[whitespace]<![x]'
        match buffer.last() {
            '!' => {
                let r = read_exact(&mut self.source, buffer, 2)?;
                if &buffer[r.clone()] != "--" {
                    return Err(ParseError::unexpected_token(&buffer[r], &["--"]).into());
                }
            }
            '-' => {
                let r = read_exact(&mut self.source, buffer, 1)?;
                if &buffer[r.clone()] != "-" {
                    return Err(ParseError::unexpected_token(&buffer[r], &["-"]).into());
                }
            }
            _ => return Err(ParseError::unexpected_token(buffer.last_str(), &["-"]).into()),
        }

        // At this point: buffer = '[whitespace]<!--'

        let mut range = buffer.len..buffer.len;
        loop {
            let r = read_until(&mut self.source, buffer, &['-'])?;
            range = range.start..r.end;

            let r = read_exact(&mut self.source, buffer, 1)?;
            if &buffer[r.clone()] == "-" {
                break;
            }
            range = range.start..r.end;
        }

        // At this point: buffer = '[whitespace]<!--[some char data]--'

        let r = read_exact(&mut self.source, buffer, 1)?;
        if &buffer[r.clone()] != ">" {
            return Err(ParseError::unexpected_token(&buffer[r], &[">"]).into());
        }

        Ok(XmlEvent::Comment(buffer[range].into()))
    }
}
