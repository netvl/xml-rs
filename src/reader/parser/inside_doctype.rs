use crate::{reader::lexer::Token, common::{is_whitespace_char, is_name_start_char, is_name_char}};

use super::{PullParser, Result, State, DoctypeSubstate, QuoteToken};

impl PullParser {
    pub fn inside_doctype(&mut self, t: Token, substate: DoctypeSubstate) -> Option<Result> {
        match substate {
            DoctypeSubstate::Outside => match t {
                Token::TagEnd => {
                    self.into_state_continue(State::OutsideTag)
                }

                Token::MarkupDeclarationStart => {
                    self.buf.clear();
                    self.into_state_continue(State::InsideDoctype(DoctypeSubstate::InsideName))
                },
                // TODO: parse SYSTEM, and [
                _ => None,
            },
            DoctypeSubstate::InsideName => match t {
                Token::Character(c @ 'A'..='Z') => {
                    self.buf.push(c);
                    None
                },
                Token::Character(c) if is_whitespace_char(c) => {
                    match self.buf.as_str() {
                        "ENTITY" => self.into_state_continue(State::InsideDoctype(DoctypeSubstate::BeforeEntityName)),
                        "NOTATION" | "ELEMENT" | "ATTLIST" => self.into_state_continue(State::InsideDoctype(DoctypeSubstate::SkipDeclaration)),
                        s => Some(self_error!(self; "Unknown markup declaration: {}", s)),
                    }

                },
                _ => Some(self_error!(self; "Incomplete markup declaration: {}", t)),
            },
            DoctypeSubstate::BeforeEntityName => {
                self.data.name.clear();
                match t {
                    Token::Character(c) if is_whitespace_char(c) => None,
                    // PEDecl unsupported
                    Token::Character('%') => self.into_state_continue(State::InsideDoctype(DoctypeSubstate::SkipDeclaration)),
                    Token::Character(c) if is_name_start_char(c) => {
                        self.data.name.push(c);
                        self.into_state_continue(State::InsideDoctype(DoctypeSubstate::EntityName))
                    },
                    _ => Some(self_error!(self; "Expected entity name, found {}", t)),
                }
            },
            DoctypeSubstate::EntityName => match t {
                Token::Character(c) if is_whitespace_char(c) => {
                    self.into_state_continue(State::InsideDoctype(DoctypeSubstate::BeforeEntityValue))
                },
                Token::Character(c) if is_name_char(c) => {
                    self.data.name.push(c);
                    None
                },
                _ => Some(self_error!(self; "Expected entity name, found {}", t)),
            },
            DoctypeSubstate::BeforeEntityValue => {
                self.buf.clear();
                match t {
                    Token::Character(c) if is_whitespace_char(c) => None,
                    // SYSTEM/PUBLIC not supported
                    Token::Character('S') | Token::Character('P') => self.into_state_continue(State::InsideDoctype(DoctypeSubstate::SkipDeclaration)),
                    Token::SingleQuote | Token::DoubleQuote => {
                        self.data.quote = Some(super::QuoteToken::from_token(&t));
                        self.into_state_continue(State::InsideDoctype(DoctypeSubstate::EntityValue))
                    },
                    _ => Some(self_error!(self; "Expected entity name, found {}", t)),
                }
            },
            DoctypeSubstate::EntityValue => match t {
                Token::SingleQuote if self.data.quote != Some(QuoteToken::SingleQuoteToken) => { self.buf.push('\''); None },
                Token::DoubleQuote if self.data.quote != Some(QuoteToken::DoubleQuoteToken) => { self.buf.push('"'); None },
                Token::SingleQuote | Token::DoubleQuote => {
                    let name = self.data.take_name();
                    let val = self.take_buf();
                    self.data.quote = None;
                    self.entities.insert(name, val);
                    self.into_state_continue(State::InsideDoctype(DoctypeSubstate::SkipDeclaration)) // FIXME
                },
                Token::Character('&') => {
                    self.data.ref_data.clear();
                    self.into_state_continue(State::InsideDoctype(DoctypeSubstate::NumericReferenceStart))
                },
                Token::Character(c) => {
                    self.buf.push(c);
                    None
                },
                _ => Some(self_error!(self; "Expected entity value, found {}", t)),
            },
            DoctypeSubstate::NumericReferenceStart => match t {
                Token::Character('#') => {
                    self.into_state_continue(State::InsideDoctype(DoctypeSubstate::NumericReference))
                },
                Token::Character(c) => {
                    self.buf.push('&');
                    self.buf.push(c);
                    self.into_state_continue(State::InsideDoctype(DoctypeSubstate::EntityValue))
                },
                _ => Some(self_error!(self; "Unexpected {} in entity", t)),
            },
            DoctypeSubstate::NumericReference => match t {
                Token::Character(';') => {
                    let r = self.data.take_ref_data();
                    // https://www.w3.org/TR/xml/#sec-entexpand
                    match self.numeric_reference_from_str(&r) {
                        Ok(c) => {
                            self.buf.push(c);
                            self.into_state_continue(State::InsideDoctype(DoctypeSubstate::EntityValue))
                        }
                        Err(e) => Some(self_error!(self; e)),
                    }
                },
                Token::Character(c) => {
                    self.data.ref_data.push(c);
                    None
                },
                _ => Some(self_error!(self; "Unexpected {} in entity", t)),
            },
            DoctypeSubstate::SkipDeclaration => match t {
                Token::TagEnd => {
                    self.into_state_continue(State::InsideDoctype(DoctypeSubstate::Outside))
                },
                _ => None,
            },
        }
    }
}
