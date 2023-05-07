use crate::{reader::lexer::Token, common::is_whitespace_char};

use super::{PullParser, Result, State};

impl PullParser {
    pub fn inside_doctype(&mut self, t: Token) -> Option<Result> {
        match t {
            Token::TagEnd => {
                self.into_state_continue(State::OutsideTag)
            }

            Token::MarkupDeclarationStart => {
                self.buf.clear();
                self.into_state_continue(State::DoctypeMarkupDeclarationStart)
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

    pub fn doctype_markup_declaration_args(&mut self, t: Token) -> Option<Result> {
        match t {
            Token::TagEnd => {
                self.into_state_continue(State::InsideDoctype)
            }
            _ => None,
        }
    }

    pub fn doctype_markup_declaration_start(&mut self, t: Token) -> Option<Result> {
        match t {
            Token::TagEnd => {
                self.into_state_continue(State::InsideDoctype)
            }
            Token::Character(c @ 'A'..='Z') => {
                self.buf.push(c);
                None
            },
            Token::Character(c) if is_whitespace_char(c) => {
                match self.buf.as_str() {
                    "ENTITY" | "NOTATION" | "ELEMENT" | "ATTLIST" => self.into_state_continue(State::DoctypeMarkupDeclarationArgs),
                    s => Some(self_error!(self; "Unknown markup declaration: {}", s)),
                }

            },
            _ => Some(self_error!(self; "Incomplete markup declaration: {}", t)),
        }
    }
}
