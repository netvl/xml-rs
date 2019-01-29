use namespace;

use reader::lexer::Token;
use reader::error::SyntaxError;

use super::{Result, PullParser, State, QualifiedNameTarget, ClosingTagSubstate};

impl PullParser {
    pub fn inside_closing_tag_name(&mut self, t: Token, s: ClosingTagSubstate) -> Option<Result> {
        match s {
            ClosingTagSubstate::CTInsideName => self.read_qualified_name(t, QualifiedNameTarget::ClosingTagNameTarget, |this, token, name| {
                match name.prefix_ref() {
                    Some(prefix) if prefix == namespace::NS_XML_PREFIX ||
                                    prefix == namespace::NS_XMLNS_PREFIX =>
                        Some(self_error!(this; SyntaxError::InvalidNamePrefix(name.prefix.clone()))),
                    _ => {
                        this.data.element_name = Some(name.clone());
                        match token {
                            Token::Whitespace(_) => this.into_state_continue(State::InsideClosingTag(ClosingTagSubstate::CTAfterName)),
                            Token::TagEnd => this.emit_end_element(),
                            _ => Some(self_error!(this; SyntaxError::UnexpectedTokenInClosingTag(token)))
                        }
                    }
                }
            }),
            ClosingTagSubstate::CTAfterName => match t {
                Token::Whitespace(_) => None,  //  Skip whitespace
                Token::TagEnd => self.emit_end_element(),
                _ => Some(self_error!(self; SyntaxError::UnexpectedTokenInClosingTag(t)))
            }
        }
    }

}
