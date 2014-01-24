use std::util;

use common::{Error, HasPosition, XmlVersion, Name, is_name_start_char, is_name_char, is_whitespace_char};
use events;
use events::XmlEvent;

use pull::lexer;
use pull::lexer::{
    Token,
    PullLexer,
    ProcessingInstructionStart,
    ProcessingInstructionEnd,
    OpeningTagStart,
    ClosingTagStart,
    TagEnd,
    EmptyTagEnd,
    CommentStart,
    CommentEnd,
    Chunk,
    Character,
    Whitespace,
    CDataStart,
    CDataEnd,
    ReferenceStart,
    ReferenceEnd,
};

pub struct PullParser {
    priv st: State,
    priv buf: ~str,
    priv lexer: PullLexer,

    priv data: MarkupData,

    priv encountered_element: bool,
    priv parsed_prolog: bool,
    priv inside_whitespace: bool
}

pub fn new() -> PullParser {
    PullParser {
        st: OutsideTag,
        buf: ~"",
        lexer: lexer::new(),

        data: MarkupData {
            name: ~"",
            version: ~"",
            encoding: ~"",
            value: ~"",
            attributes: ~[]
        },

        encountered_element: false,
        parsed_prolog: false,
        inside_whitespace: true
    }
}

enum State {
    OutsideTag,
    InsideOpeningTag(ElementSubstate),
    InsideClosingTagName,
    InsideProcessingInstruction(ProcessingInstructionSubstate),
    InsideComment,
    InsideCData,
    InsideProlog(PrologSubstate),
    InsideReference
}

enum ElementSubstate {
    InsideName,
    InsideAttributeName,
    InsideAttributeValue,
}

enum ProcessingInstructionSubstate {
    PIInsideName,
    PIInsideData
}

enum PrologSubstate {
    BeforeVersion,
    InsideVersion,
    InsideEncoding,
    InsideStandaloneDecl
}

struct AttributeData {
    name: ~str,
    value: ~str
}

struct MarkupData {
    name: ~str,
    version: ~str,
    encoding: ~str,
    value: ~str,
    attributes: ~[AttributeData]
}

macro_rules! gen_takes(
    ($($field:ident -> $method:ident)+) => (
        $(
        impl MarkupData {
            #[inline]
            fn $method(&mut self) -> ~str {
                util::replace(&mut self.$field, ~"")
            }
        }
        )+
    )
)

gen_takes!(
    name     -> take_name
    version  -> take_version
    encoding -> take_encoding
    value    -> take_value
)

macro_rules! self_error(
    ($msg:expr) => (
        self.error($msg.to_owned())
    );
    ($fmt:expr, $($arg:expr),+) => (
        self.error(format!($fmt, $($arg),+))
    )
)

impl PullParser {
    pub fn next<B: Buffer>(&mut self, r: &mut B) -> XmlEvent {
        for_each!(t in self.lexer.next_token(r) {
            match t {
                Ok(t) => match self.dispatch_token(t) {
                    Some(ev) => return ev,
                    None => {}  // continue
                },
                Err(e) => return events::Error(e)
            }
        })
        self_error!("Unexpected end of stream")
    }

    fn error(&self, msg: ~str) -> XmlEvent {
        events::Error(Error::new(&self.lexer, msg))
    }

    fn dispatch_token(&mut self, t: Token) -> Option<XmlEvent> {
        match self.st {
            OutsideTag                     => self.outside_tag(t),
            InsideProcessingInstruction(s) => self.inside_processing_instruction(t, s),
            InsideProlog(s)                => self.inside_prolog(t, s),
            InsideOpeningTag(s)            => self.inside_opening_tag(t, s),
            InsideClosingTagName           => self.inside_closing_tag_name(t),
            InsideComment                  => self.inside_comment(t),
            InsideCData                    => self.inside_cdata(t),
            InsideReference                => self.inside_reference(t)
        }
    }

    #[inline]
    fn buf_has_data(&self) -> bool {
        self.buf.len() > 0
    }

    #[inline]
    fn take_buf(&mut self) -> ~str {
        util::replace(&mut self.buf, ~"")
    } 
    
    #[inline]
    fn into_state(&mut self, st: State, ev: Option<XmlEvent>) -> Option<XmlEvent> {
        self.st = st;
        ev
    }

    #[inline]
    fn into_state_continue(&mut self, st: State) -> Option<XmlEvent> {
        self.into_state(st, None)
    }

    #[inline]
    fn into_state_emit(&mut self, st: State, ev: XmlEvent) -> Option<XmlEvent> {
        self.into_state(st, Some(ev))
    }

    fn outside_tag(&mut self, t: Token) -> Option<XmlEvent> {
        match t {
            ReferenceStart =>
                self.into_state_continue(InsideReference),

            Whitespace(c) => {
                self.buf.push_char(c);
                None
            }

            _ if t.contains_char_data() => {  // Non-whitespace char data
                self.inside_whitespace = false;
                self.buf.push_str(t.to_str());
                None
            }

            _ => {  // Encountered some meaningful event, flush the buffer as an event
                let next_event = if self.buf_has_data() {
                    let buf = self.take_buf();
                    // TODO: perform unescaping? or it is done in reference processing?
                    Some(if self.inside_whitespace {
                        events::Whitespace(buf)
                    } else {
                        events::Characters(buf)
                    })
                } else { None };
                self.inside_whitespace = true;  // Reset inside_whitespace flag
                match t {
                    ProcessingInstructionStart => 
                        self.into_state(InsideProcessingInstruction(PIInsideName), next_event),

                    OpeningTagStart =>
                        self.into_state(InsideOpeningTag(InsideName), next_event),

                    ClosingTagStart =>
                        self.into_state(InsideClosingTagName, next_event),

                    CommentStart => {
                        // We need to disable lexing errors inside comments
                        self.lexer.disable_errors();
                        self.into_state(InsideComment, next_event)
                    }

                    CDataStart => {
                        // We need to disable lexing errors inside comments
                        self.lexer.disable_errors();
                        self.into_state(InsideCData, next_event)
                    }

                    _ => Some(self_error!("Unexpected token: {}", t.to_str()))
                }
            }
        }
    }

    fn inside_processing_instruction(&mut self, t: Token, s: ProcessingInstructionSubstate) -> Option<XmlEvent> {
        match s {
            PIInsideName => match t {
                Character(c) if !self.buf_has_data() && is_name_start_char(c) => {
                    self.buf.push_char(c);
                    None
                }

                Character(c) if self.buf_has_data() && is_name_char(c) => {
                    self.buf.push_char(c);
                    None
                }

                ProcessingInstructionEnd => {
                    // self.buf contains PI name
                    let name = self.take_buf();
                       
                    // Don't need to check for prolog because it has mandatory attributes
                    // but there is none
                    match name.as_slice() {
                        // Found <?xml-like PI after the beginning of a document,
                        // it is an error - see section 2.6 of XML 1.1 spec
                        "xml"|"xmL"|"xMl"|"xML"|"Xml"|"XmL"|"XMl"|"XML"
                            if self.encountered_element || self.parsed_prolog =>
                            Some(self_error!("Invalid processing instruction: <?{}", name)),

                        // Name is ok, emitting event
                        _ => {
                            self.into_state_emit(
                                OutsideTag, 
                                events::ProcessingInstruction {
                                    name: name,
                                    data: None
                                }
                            )
                        }
                    }
                }

                Whitespace(c) => {
                    // self.buf contains PI name
                    let name = self.take_buf();

                    match name.as_slice() {
                        // We have not ever encountered an element and have not parsed prolog
                        "xml" if !self.encountered_element && !self.parsed_prolog =>
                            self.into_state_continue(InsideProlog(BeforeVersion)),

                        // Found <?xml-like PI after the beginning of a document,
                        // it is an error - see section 2.6 of XML 1.1 spec
                        "xml"|"xmL"|"xMl"|"xML"|"Xml"|"XmL"|"XMl"|"XML"
                            if self.encountered_element || self.parsed_prolog =>
                            Some(self_error!("Invalid processing instruction: <?{}", name)),

                        // Name is ok, starting parsing data
                        _ => {
                            self.lexer.disable_errors();  // data is arbitrary, so disable errors
                            self.data.name = name;
                            self.into_state_continue(InsideProcessingInstruction(PIInsideData))
                        }

                    }
                }

                _ => Some(self_error!("Unexpected token: <?{}{}", self.buf, t.to_str()))
            },
 
            PIInsideData => match t {
                ProcessingInstructionEnd => {
                    self.lexer.enable_errors();
                    let name = self.data.take_name();
                    let data = self.take_buf();
                    self.into_state_emit(
                        OutsideTag, 
                        events::ProcessingInstruction {
                            name: name,
                            data: Some(data)
                        }
                    )
                },

                // Any other token should be treated as plain characters
                _ => {
                    self.buf.push_str(t.to_str());
                    None
                }
            },
        }
    }

    fn inside_prolog(&mut self, t: Token, s: PrologSubstate) -> Option<XmlEvent> {
        None
    }

    fn inside_opening_tag(&mut self, t: Token, s: ElementSubstate) -> Option<XmlEvent> {
        None
    }

    fn inside_closing_tag_name(&mut self, t: Token) -> Option<XmlEvent> {
        None
    }

    fn inside_comment(&mut self, t: Token) -> Option<XmlEvent> {
        None
    }

    fn inside_cdata(&mut self, t: Token) -> Option<XmlEvent> {
        None
    }

    fn inside_reference(&mut self, t: Token) -> Option<XmlEvent> {
        None
    }
}

#[cfg(test)]
mod tests {
    use std::io::mem::MemReader;

    use super::PullParser;
}
