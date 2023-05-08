use crate::{reader::events::XmlEvent, common::is_whitespace_char};
use crate::reader::lexer::Token;

use super::{PullParser, Result, State};

impl PullParser {
    pub fn inside_cdata(&mut self, t: Token) -> Option<Result> {
        match t {
            Token::CDataEnd => {
                let event = if self.config.c.cdata_to_characters {
                    None
                } else {
                    let data = self.take_buf();
                    Some(Ok(XmlEvent::CData(data)))
                };
                self.into_state(State::OutsideTag, event)
            }

            Token::Character(c) => {
                if !is_whitespace_char(c) {
                    self.inside_whitespace = false;
                }
                self.buf.push(c);
                None
            }

            _ => unreachable!(),
        }
    }
}
