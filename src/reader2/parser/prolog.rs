use std::io::Read;

use crate::chars::is_whitespace_str;
use crate::event::{XmlEvent, XmlVersion};

use super::super::error::{InvalidDeclarationReason, ParseError, Result};
use super::super::Buffer;
use super::attributes::Attributes;
use super::util::*;
use super::{Parser, PrologSubstate, State};

impl<R: Read> Parser<R> {
    pub(super) fn parse_prolog<'buf>(
        &mut self,
        substate: PrologSubstate,
        buffer: &'buf mut Buffer,
    ) -> Result<XmlEvent<'buf>> {
        debug_assert!(buffer.is_empty());

        let r = read_until(&mut self.source, buffer, &['<'])?;

        match substate {
            PrologSubstate::BeforeDeclaration if is_whitespace_str(&buffer[r.clone()]) => {
                self.state = State::Prolog(PrologSubstate::BeforeDoctype);
            }
            PrologSubstate::BeforeDoctype | PrologSubstate::BeforeDocument if is_whitespace_str(&buffer[r.clone()]) => {
            }
            _ if !buffer[r.clone()].is_empty() => {
                return Err(ParseError::unexpected_token(&buffer[r], &["<", "whitespace"]).into());
            }
            _ => {}
        }

        let r = read_exact(&mut self.source, buffer, 1)?;
        match &buffer[r] {
            "?" => {
                let r = read_up_to(&mut self.source, buffer, 3)?;
                if &buffer[r] == "xml" {
                    if substate == PrologSubstate::BeforeDeclaration {
                        self.state = State::Prolog(PrologSubstate::BeforeDoctype);
                        self.parse_declaration(buffer)
                    } else {
                        Err(ParseError::UnexpectedDeclaration.into())
                    }
                } else {
                    if substate == PrologSubstate::BeforeDeclaration {
                        self.state = State::Prolog(PrologSubstate::BeforeDoctype);
                    }
                    self.parse_processing_instruction(buffer)
                }
            }
            "!" => {
                let r = read_exact(&mut self.source, buffer, 1)?;
                if &buffer[r.clone()] == "-" {
                    self.parse_comment(buffer)
                } else if &buffer[r.clone()] == "D" {
                    if substate <= PrologSubstate::BeforeDoctype {
                        self.state = State::Prolog(PrologSubstate::BeforeDocument);
                        self.parse_doctype(buffer)
                    } else {
                        let r = read_up_to(&mut self.source, buffer, 6)?;
                        match &buffer[r] {
                            "OCTYPE" => Err(ParseError::UnexpectedDoctype.into()),
                            t => Err(ParseError::unexpected_token("D", &["-"]).into()),
                        }
                    }
                } else {
                    Err(ParseError::unexpected_token(&buffer[r], &["-", "DOCTYPE"]).into())
                }
            }
            _ => {
                self.state = State::OutsideTag;
                let result = self.parse_start_element(buffer)?;
                Ok(result)
            }
        }
    }

    fn parse_declaration<'buf>(&mut self, buffer: &'buf mut Buffer) -> Result<XmlEvent<'buf>> {
        // At this point: buffer == '[whitespace]<?xml'

        // find the end
        let content_r = read_until(&mut self.source, buffer, &['?'])?;

        let r = read_exact(&mut self.source, buffer, 1)?;
        if &buffer[r.clone()] != ">" {
            return Err(ParseError::unexpected_token(&buffer[r], &['>']).into());
        }

        let mut attributes = Attributes::new(&buffer[content_r], '?');

        let first = match attributes.next() {
            Some(attr) => attr?,
            None => {
                return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::MissingVersion).into());
            }
        };

        if first.name.local_name != "version" || first.name.prefix.is_some() {
            return Err(
                ParseError::InvalidDeclaration(InvalidDeclarationReason::unexpected_content(first.name.to_string()))
                    .into(),
            );
        }

        let version = match &first.value[..] {
            "1.0" => XmlVersion::Version10,
            "1.1" => XmlVersion::Version11,
            other => {
                return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::invalid_version(other)).into());
            }
        };

        let second = match attributes.next() {
            Some(attr) => attr?,
            None => return Ok(XmlEvent::start_document(version, "UTF-8", None)),
        };

        if second.name.prefix.is_some()
            || (second.name.local_name != "encoding" && second.name.local_name != "standalone")
        {
            return Err(
                ParseError::InvalidDeclaration(InvalidDeclarationReason::unexpected_content(second.name.to_string()))
                    .into(),
            );
        }

        let mut encoding = "UTF-8".into();
        let mut standalone = None;

        if second.name.local_name == "encoding" {
            encoding = second.value.clone();
        }
        if second.name.local_name == "standalone" {
            standalone = Some(match &second.value[..] {
                "yes" => true,
                "no" => false,
                other => {
                    return Err(
                        ParseError::InvalidDeclaration(InvalidDeclarationReason::invalid_standalone(other)).into(),
                    );
                }
            });
        }

        let third = match attributes.next() {
            Some(attr) => {
                if standalone.is_some() {
                    return Err(
                        ParseError::InvalidDeclaration(InvalidDeclarationReason::unexpected_content(
                            attr?.name.to_string(),
                        ))
                        .into(),
                    );
                } else {
                    attr?
                }
            }
            None => return Ok(XmlEvent::start_document(version, encoding, standalone)),
        };

        if third.name.local_name != "standalone" || third.name.prefix.is_some() {
            return Err(
                ParseError::InvalidDeclaration(InvalidDeclarationReason::unexpected_content(third.name.to_string()))
                    .into(),
            );
        }

        standalone = Some(match &third.value[..] {
            "yes" => true,
            "no" => false,
            other => {
                return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::invalid_standalone(other)).into());
            }
        });

        Ok(XmlEvent::start_document(version, encoding, standalone))
    }
}
