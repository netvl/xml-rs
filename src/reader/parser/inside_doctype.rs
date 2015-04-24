use reader::events::XmlEvent;
use reader::lexer::Token;

use super::{PullParser, State};

impl PullParser {
    pub fn inside_doctype(&mut self, t: Token) -> Option<XmlEvent> {
        match t {
            Token::TagEnd => {
                self.lexer.enable_errors();
                self.into_state_continue(State::OutsideTag)
            }

            _ => None
        }
    }
}
