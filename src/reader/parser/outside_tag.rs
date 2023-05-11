use crate::reader::error::SyntaxError;
use crate::common::is_whitespace_char;
use crate::reader::events::XmlEvent;
use crate::reader::lexer::Token;

use super::{
    ClosingTagSubstate, DoctypeSubstate, Encountered, OpeningTagSubstate,
    ProcessingInstructionSubstate, PullParser, Result, State,
};

impl PullParser {
    pub fn outside_tag(&mut self, t: Token) -> Option<Result> {
        match t {
            Token::ReferenceStart if self.depth() > 0 => {
                self.state_after_reference = State::OutsideTag;
                self.into_state_continue(State::InsideReference)
            },

            Token::Character(c) if is_whitespace_char(c) => {
                // skip whitespace outside of the root element
                if self.depth() == 0 && self.config.c.ignore_root_level_whitespace { None }
                else if self.config.c.trim_whitespace && !self.buf_has_data() { None }
                else {
                    if !self.buf_has_data() {
                        self.push_pos();
                    }
                    self.append_char_continue(c)
                }
            }

            _ if t.contains_char_data() && self.depth() == 0 => {
                Some(self.error(SyntaxError::UnexpectedTokenOutsideRoot(t)))
            }

            Token::CDataEnd => Some(self.error(SyntaxError::UnexpectedCdataEnd)),

            Token::ReferenceEnd if self.depth() > 0 => { // Semi-colon in a text outside an entity
                self.inside_whitespace = false;
                Token::ReferenceEnd.push_to_string(&mut self.buf);
                None
            },

            _ if t.contains_char_data() => {  // Non-whitespace char data
                if !self.buf_has_data() {
                    self.push_pos();
                }
                self.inside_whitespace = false;
                t.push_to_string(&mut self.buf);
                None
            }

            Token::CommentStart if self.config.c.coalesce_characters && self.config.c.ignore_comments => {
                let next_event = self.set_encountered(Encountered::Comment);
                // We need to switch the lexer into a comment mode inside comments
                self.into_state(State::InsideComment, next_event)
            }

            Token::CDataStart if self.depth() > 0 && self.config.c.coalesce_characters && self.config.c.cdata_to_characters => {
                if !self.buf_has_data() {
                    self.push_pos();
                }
                // We need to disable lexing errors inside CDATA
                self.into_state_continue(State::InsideCData)
            }

            _ => {
                // Encountered some markup event, flush the buffer as characters
                // or a whitespace
                let mut next_event = if self.buf_has_data() {
                    let buf = self.take_buf();
                    if self.inside_whitespace && self.config.c.trim_whitespace {
                        None
                    } else if self.inside_whitespace && !self.config.c.whitespace_to_characters {
                        Some(Ok(XmlEvent::Whitespace(buf)))
                    } else if self.config.c.trim_whitespace {
                        Some(Ok(XmlEvent::Characters(buf.trim_matches(is_whitespace_char).into())))
                    } else {
                        Some(Ok(XmlEvent::Characters(buf)))
                    }
                } else { None };
                self.inside_whitespace = true;  // Reset inside_whitespace flag
                self.push_pos();
                match t {
                    Token::ProcessingInstructionStart =>
                        self.into_state(State::InsideProcessingInstruction(ProcessingInstructionSubstate::PIInsideName), next_event),

                    Token::DoctypeStart if self.encountered < Encountered::Doctype => {
                        if let Some(e) = self.set_encountered(Encountered::Doctype) {
                            next_event = Some(e);
                        }

                        // We don't have a doctype event so skip this position
                        // FIXME: update when we have a doctype event
                        self.next_pos();
                        self.into_state(State::InsideDoctype(DoctypeSubstate::Outside), next_event)
                    }

                    Token::OpeningTagStart if self.depth() > 0 || self.encountered < Encountered::Element || self.config.allow_multiple_root_elements => {
                        if let Some(e) = self.set_encountered(Encountered::Element) {
                            next_event = Some(e);
                        }
                        self.nst.push_empty();
                        self.into_state(State::InsideOpeningTag(OpeningTagSubstate::InsideName), next_event)
                    }

                    Token::ClosingTagStart if self.depth() > 0 =>
                        self.into_state(State::InsideClosingTag(ClosingTagSubstate::CTInsideName), next_event),

                    Token::CommentStart => {
                        if let Some(e) = self.set_encountered(Encountered::Comment) {
                            next_event = Some(e);
                        }
                        // We need to switch the lexer into a comment mode inside comments
                        self.into_state(State::InsideComment, next_event)
                    }

                    Token::CDataStart if self.depth() > 0 => {
                        self.into_state(State::InsideCData, next_event)
                    }

                    _ => Some(self.error(SyntaxError::UnexpectedToken(t)))
                }
            }
        }
    }
}
