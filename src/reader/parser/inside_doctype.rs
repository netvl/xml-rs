use crate::reader::lexer::Token;

use super::{PullParser, Result, State};

impl PullParser {
    pub fn inside_doctype(&mut self, t: Token) -> Option<Result> {
        match t {
            Token::TagEnd => {
                self.into_state_continue(State::OutsideTag)
            }

            Token::MarkupDeclarationStart => {
                self.into_state_continue(State::InsideDoctypeMarkupDeclaration)
            },

            _ => None,
        }
    }

    pub fn inside_doctype_markup_declaration(&mut self, t: Token) -> Option<Result> {
        match t {
            Token::TagEnd => {
                self.into_state_continue(State::InsideDoctype)
            }

            _ => None,
        }
    }
}
