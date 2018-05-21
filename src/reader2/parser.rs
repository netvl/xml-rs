use std::io::Read;
use std::ops::{Range, RangeFrom};

use reader2::error::{Result, ParseError, InvalidDeclarationReason};
#[cfg(feature = "encodings")]
use reader2::encodings::DelimitingReader;
use name2::Name;
use attribute2::Attribute;
use event::{XmlEvent, XmlVersion};
use position::{TextPosition, Position};
use chars::{is_whitespace_str, is_whitespace_char, is_name_start_char, is_name_char};

pub struct Parser<R: Read> {
    source: DelimitingReader<R>,
    buffer: String,
    state: State,
    pos: TextPosition,
}

impl<R: Read> Position for Parser<R> {
    fn position(&self) -> TextPosition {
        self.pos
    }
}

enum State {
    Prolog(PrologSubstate),
    OutsideTag,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum PrologSubstate {
    BeforeDeclaration,
    BeforeDoctype,
    BeforeDocument,
}

const BUFFER_SIZE: usize = 8192;

impl<R: Read> Parser<R> {
    #[cfg(feature = "encodings")]
    pub fn new(source: R) -> Parser<R> {
        use encoding_rs::UTF_8;

        Parser {
            source: DelimitingReader::new(source, UTF_8, BUFFER_SIZE, BUFFER_SIZE),
            buffer: String::new(),
            state: State::Prolog(PrologSubstate::BeforeDeclaration),
            pos: TextPosition::new(),
        }
    }

    pub fn next<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        buffer.clear();
        match self.state {
            State::Prolog(substate) => self.parse_prolog(substate, buffer),
            State::OutsideTag => self.parse_outside_tag(buffer),
        }
    }

    fn parse_prolog<'buf>(&mut self, substate: PrologSubstate, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        // At this point: buffer is empty
        let r = read_until(&mut self.source, buffer, &['<'])?;

        match substate {
            PrologSubstate::BeforeDeclaration if is_whitespace_str(&buffer[r.clone()]) => {
                self.state = State::Prolog(PrologSubstate::BeforeDoctype);
            }
            PrologSubstate::BeforeDoctype | PrologSubstate::BeforeDocument if is_whitespace_str(&buffer[r.clone()]) => {}
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
                        self.parse_declaration(buffer)
                    } else {
                        Err(ParseError::UnexpectedDeclaration.into())
                    }
                } else {
                    self.parse_processing_instruction(buffer)
                }
            },
            "!" => {
                let r = read_exact(&mut self.source, buffer, 1)?;
                if &buffer[r.clone()] == "-" {
                    self.parse_comment(buffer)
                } else if &buffer[r.clone()] == "D" {
                    if substate <= PrologSubstate::BeforeDoctype {
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
            },
            _ => {
                self.state = State::OutsideTag;
                let result = self.parse_start_element(buffer)?;
                Ok(result)
            }
        }
    }

    fn parse_outside_tag<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        unimplemented!()
    }

    fn parse_processing_instruction<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        unimplemented!()
    }

    fn parse_comment<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        unimplemented!()
    }

    fn parse_declaration<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
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
            None => return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::MissingVersion).into()),
        };

        if first.name.local_name != "version" || first.name.prefix.is_some() {
            return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::unexpected_content(first.name.to_string())).into());
        }

        let version = match &first.value[..] {
            "1.0" => XmlVersion::Version10,
            "1.1" => XmlVersion::Version11,
            other => return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::invalid_version(other)).into()),
        };

        let second = match attributes.next() {
            Some(attr) => attr?,
            None => return Ok(XmlEvent::StartDocument {
                version,
                encoding: "UTF-8".into(),
                standalone: None,
            }),
        };

        if second.name.prefix.is_some() || (second.name.local_name != "encoding" && second.name.local_name != "standalone") {
            return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::unexpected_content(second.name.to_string())).into());
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
                other => return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::invalid_standalone(other)).into()),
            });
        }

        let third = match attributes.next() {
            Some(attr) => if standalone.is_some() {
                return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::unexpected_content(attr?.name.to_string())).into());
            } else {
                attr?
            },
            None => return Ok(XmlEvent::StartDocument {
                version,
                encoding,
                standalone,
            }),
        };

        if third.name.local_name != "standalone" || third.name.prefix.is_some() {
            return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::unexpected_content(third.name.to_string())).into());
        }

        standalone = Some(match &third.value[..] {
            "yes" => true,
            "no" => false,
            other => return Err(ParseError::InvalidDeclaration(InvalidDeclarationReason::invalid_standalone(other)).into()),
        });

        Ok(XmlEvent::StartDocument {
            version,
            encoding,
            standalone,
        })
    }

    fn parse_doctype<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        unimplemented!()
    }

    fn parse_start_element<'buf>(&mut self, buffer: &'buf mut String) -> Result<XmlEvent<'buf>> {
        unimplemented!()
    }
}

// never returns empty slice
fn read_up_to<'buf, R>(source: &mut DelimitingReader<R>,
                       buffer: &'buf mut String,
                       n: usize) -> Result<RangeFrom<usize>>
    where R: Read,
{
    let len_before = buffer.len();
    source.read_exact_chars(n, buffer)?;
    if len_before == buffer.len() {
        Err(ParseError::unexpected_eof_no_expected().into())
    } else {
        Ok(len_before..)
    }
}

// never returns empty slice
fn read_exact<R>(source: &mut DelimitingReader<R>,
                 buffer: &mut String,
                 n: usize) -> Result<RangeFrom<usize>>
    where R: Read,
{
    if source.read_exact_chars(n, buffer)? {
        Ok(buffer.len() - n..)
    } else {
        Err(ParseError::unexpected_eof_no_expected().into())
    }
}

// Reads from the source until encountered one of `chars`
// Returns a slice of the buffer corresponding to what was read excluding the matching char
fn read_until<R>(source: &mut DelimitingReader<R>,
                 buffer: &mut String,
                 chars: &[char]) -> Result<Range<usize>>
    where R: Read,
{
    let len_before = buffer.len();
    if source.read_until(chars, buffer)? {
        Ok(len_before..buffer.len()-1)
    } else {
        Err(ParseError::unexpected_eof(Some(chars)).into())
    }
}

// Expects buffer to contain only key='value'/key="value" pairs, possibly separated by whitespace
struct Attributes<'buf> {
    buffer: &'buf str,
    next_char: char,
}

impl<'buf> Attributes<'buf> {
    fn new(buffer: &'buf str, next_char: char) -> Attributes<'buf> {
        Attributes { buffer, next_char, }
    }

    fn parse(buffer: &'buf str, next_char: char) -> Result<Vec<Attribute<'buf>>> {
        Attributes::new(buffer, next_char).collect()
    }

    fn next_unexpected_token<T>(&self, expected: &[&str]) -> Result<T> {
        Err(ParseError::unexpected_token(self.next_char.to_string(), expected).into())
    }

    fn first_unexpected_token<T>(&self, expected: &[&str]) -> Result<T> {
        Err(ParseError::unexpected_token(self.buffer.chars().next().unwrap().to_string(), expected).into())
    }
}

impl<'buf> Iterator for Attributes<'buf> {
    type Item = Result<Attribute<'buf>>;

    // TODO: replace predicates with literal matches for whitespace, which can potentially be faster
    fn next(&mut self) -> Option<Result<Attribute<'buf>>> {
        if self.buffer.is_empty() || is_whitespace_str(self.buffer) {
            return None;
        }

        let attr_name_start = match self.buffer.find(is_name_start_char) {
            Some(idx) => idx,
            None => return Some(self.first_unexpected_token(&["name start character"])),
        };
        if !self.buffer[..attr_name_start].is_empty() && !is_whitespace_str(&self.buffer[..attr_name_start]) {
            return Some(self.first_unexpected_token(&["name start character"]));
        }

        self.buffer = &self.buffer[attr_name_start..];

        let attr_name_end = match self.buffer.find(|c| is_whitespace_char(c) || c == '=') {
            Some(idx) => idx,
            None => return Some(self.next_unexpected_token(&["=", "whitespace"])),
        };

        let attr_name = &self.buffer[..attr_name_end];
        if let Some(idx) = attr_name.find(|c| !is_name_char(c)) {
            return Some(self.first_unexpected_token(&["name character"]));
        }
        let attr_name = match Name::from_str(attr_name) {
            Some(name) => name,
            None => return Some(Err(ParseError::invalid_name(attr_name).into())),
        };

        self.buffer = &self.buffer[attr_name_end..];

        if !self.buffer.starts_with("=") {
            let eq_idx = match self.buffer.find('=') {
                Some(idx) => idx,
                None => return Some(self.next_unexpected_token(&["="])),
            };

            if !self.buffer[..eq_idx].is_empty() && !is_whitespace_str(&self.buffer[..eq_idx]) {
                return Some(self.first_unexpected_token(&["=", "whitespace"]));
            }

            self.buffer = &self.buffer[eq_idx + 1..];
        } else {
            self.buffer = &self.buffer[1..];
        }

        let quote_start = match self.buffer.find(|c| c == '\'' || c == '"') {
            Some(idx) => idx,
            None => return Some(self.next_unexpected_token(&["'", "\""])),
        };
        let quote_char = self.buffer.chars().next().unwrap();

        self.buffer = &self.buffer[quote_start + 1..];
        let quote_end = match self.buffer.find(quote_char) {
            Some(idx) => idx,
            None => return Some(Err(ParseError::UnclosedAttributeValue.into())),
        };

        let attr_value = &self.buffer[..quote_end];

        // TODO: validate value and expand character entities in it

        self.buffer = &self.buffer[quote_end + 1..];

        Some(Ok(Attribute {
            name: attr_name,
            value: attr_value.into(),
        }))
    }
}
