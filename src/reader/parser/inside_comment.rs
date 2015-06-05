use reader::events::XmlEvent;
use reader::lexer::Token;

use super::{PullParser, State};

impl PullParser {
    pub fn inside_comment(&mut self, t: Token) -> Option<XmlEvent> {
        match t {
            // Double dash is illegal inside a comment
            Token::Chunk(ref s) if &s[..] == "--" => Some(self_error!(self; "Unexpected token inside a comment: --")),

            Token::CommentEnd if self.config.ignore_comments => {
                self.lexer.enable_errors();
                self.into_state_continue(State::OutsideTag)
            }

            Token::CommentEnd => {
                self.lexer.enable_errors();
                let data = self.take_buf();
                self.into_state_emit(State::OutsideTag, XmlEvent::Comment(data))
            }

            _ if self.config.ignore_comments => None,  // Do not modify buffer if ignoring the comment

            _ => {
                t.push_to_string(&mut self.buf);
                None
            }
        }
    }

}
