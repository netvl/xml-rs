use crate::reader::error::SyntaxError;
use crate::common::{is_name_char, is_name_start_char, is_whitespace_char};

use crate::reader::events::XmlEvent;
use crate::reader::lexer::Token;

use super::{DeclarationSubstate, ProcessingInstructionSubstate, PullParser, Result, State, Encountered};

impl PullParser {
    pub fn inside_processing_instruction(&mut self, t: Token, s: ProcessingInstructionSubstate) -> Option<Result> {
        match s {
            ProcessingInstructionSubstate::PIInsideName => match t {
                Token::Character(c) if !self.buf_has_data() && is_name_start_char(c) ||
                                 self.buf_has_data() && is_name_char(c) => self.append_char_continue(c),

                Token::ProcessingInstructionEnd => {
                    // self.buf contains PI name
                    let name = self.take_buf();

                    // Don't need to check for declaration because it has mandatory attributes
                    // but there is none
                    match &*name {
                        // Name is empty, it is an error
                        "" => Some(self.error(SyntaxError::ProcessingInstructionWithoutName)),

                        // Found <?xml-like PI not at the beginning of a document,
                        // it is an error - see section 2.6 of XML 1.1 spec
                        "xml"|"xmL"|"xMl"|"xML"|"Xml"|"XmL"|"XMl"|"XML" =>
                            Some(self.error(SyntaxError::InvalidXmlProcessingInstruction(name.into()))),

                        // All is ok, emitting event
                        _ => {
                            self.into_state_emit(
                                State::OutsideTag,
                                Ok(XmlEvent::ProcessingInstruction {
                                    name,
                                    data: None
                                })
                            )
                        }
                    }
                }

                Token::Character(c) if is_whitespace_char(c) => {
                    // self.buf contains PI name
                    let name = self.take_buf();

                    match &*name {
                        // We have not ever encountered an element and have not parsed XML declaration
                        "xml" if self.encountered == Encountered::None =>
                            self.into_state_continue(State::InsideDeclaration(DeclarationSubstate::BeforeVersion)),

                        // Found <?xml-like PI after the beginning of a document,
                        // it is an error - see section 2.6 of XML 1.1 spec
                        "xml"|"xmL"|"xMl"|"xML"|"Xml"|"XmL"|"XMl"|"XML" =>
                            Some(self.error(SyntaxError::InvalidProcessingInstruction(name.into()))),

                        // All is ok, starting parsing PI data
                        _ => {
                            self.data.name = name;
                            self.into_state_continue(State::InsideProcessingInstruction(ProcessingInstructionSubstate::PIInsideData))
                        }
                    }
                }

                _ => {
                    let buf = self.take_buf();
                    Some(self.error(SyntaxError::UnexpectedProcessingInstruction(buf.into(), t)))
                }
            },

            ProcessingInstructionSubstate::PIInsideData => match t {
                Token::ProcessingInstructionEnd => {
                    let name = self.data.take_name();
                    let data = self.take_buf();
                    self.into_state_emit(
                        State::OutsideTag,
                        Ok(XmlEvent::ProcessingInstruction {
                            name,
                            data: Some(data),
                        }),
                    )
                },

                // Any other token should be treated as plain characters
                _ => {
                    t.push_to_string(&mut self.buf);
                    None
                }
            },
        }
    }
}
