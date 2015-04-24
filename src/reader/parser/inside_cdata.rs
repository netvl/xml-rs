use reader::events::XmlEvent;
use reader::lexer::Token;

use super::{PullParser, State};

impl PullParser {
    pub fn inside_cdata(&mut self, t: Token) -> Option<XmlEvent> {
        match t {
            Token::CDataEnd => {
                self.lexer.enable_errors();
                let event = if self.config.cdata_to_characters {
                    None
                } else {
                    let data = self.take_buf();
                    Some(XmlEvent::CData(data))
                };
                self.into_state(State::OutsideTag, event)
            }

            Token::Whitespace(_) => self.append_str_continue(&t.to_string()),

            _ => {
                self.inside_whitespace = false;
                self.append_str_continue(&t.to_string())
            }
        }
    }
}
