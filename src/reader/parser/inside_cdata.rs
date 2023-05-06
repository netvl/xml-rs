use crate::reader::events::XmlEvent;
use crate::reader::lexer::Token;

use super::{PullParser, Result, State};

impl PullParser {
    pub fn inside_cdata(&mut self, t: Token) -> Option<Result> {
        match t {
            Token::CDataEnd => {
                let event = if self.config.cdata_to_characters {
                    None
                } else {
                    let data = self.take_buf();
                    Some(Ok(XmlEvent::CData(data)))
                };
                self.into_state(State::OutsideTag, event)
            }

            Token::Whitespace(c) => {
                self.buf.push(c);
                None
            }

            Token::Character(c) => {
                self.inside_whitespace = false;
                self.buf.push(c);
                None
            }

            Token::Chunk(s) => {
                self.inside_whitespace = false;
                self.buf.push_str(s);
                None
            }

            _ => unreachable!(),
        }
    }
}
