use std::util;

use common;
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
    DoubleQuote,
    SingleQuote,
    EqualsSign
};

pub struct PullParser {
    priv st: State,
    priv buf: ~str,
    priv lexer: PullLexer,

    priv data: MarkupData,
    priv finish_event: Option<XmlEvent>,
    priv next_event: Option<XmlEvent>,
    priv depth: uint,

    priv encountered_element: bool,
    priv parsed_declaration: bool,
    priv inside_whitespace: bool
}

pub fn new() -> PullParser {
    PullParser {
        st: OutsideTag,
        buf: ~"",
        lexer: lexer::new(),

        data: MarkupData {
            name: ~"",
            version: None,
            encoding: ~"",
            standalone: None,
            value: ~"",
            ref_data: ~"",
            quote: None,
            attributes: ~[]
        },
        finish_event: None,
        next_event: None,
        depth: 0,

        encountered_element: false,
        parsed_declaration: false,
        inside_whitespace: true
    }
}

#[deriving(Clone, Eq)]
enum State {
    OutsideTag,
    InsideOpeningTag(ElementSubstate),
    InsideClosingTagName,
    InsideProcessingInstruction(ProcessingInstructionSubstate),
    InsideComment,
    InsideCData,
    InsideDeclaration(DeclarationSubstate),
    InsideReference(~State)
}

#[deriving(Clone, Eq)]
enum ElementSubstate {
    InsideName,

    InsideTag,

    InsideAttributeName,
    AfterAttributeName,

    InsideAttributeValue,
}

#[deriving(Clone, Eq)]
enum ProcessingInstructionSubstate {
    PIInsideName,
    PIInsideData
}

#[deriving(Clone, Eq)]
enum DeclarationSubstate {
    BeforeVersion,
    InsideVersion,
    AfterVersion,

    InsideVersionValue,
    AfterVersionValue,

    InsideEncoding,
    AfterEncoding,

    InsideEncodingValue,

    BeforeStandaloneDecl,
    InsideStandaloneDecl,
    AfterStandaloneDecl,

    InsideStandaloneDeclValue,
    AfterStandaloneDeclValue
}

struct AttributeData {
    name: Name,
    value: ~str
}

struct MarkupData {
    name: ~str,
    version: Option<common::XmlVersion>,
    encoding: ~str,
    standalone: Option<bool>,
    value: ~str,
    ref_data: ~str,
    quote: Option<Token>,
    attributes: ~[AttributeData]
}

macro_rules! gen_takes(
    ($($field:ident -> $method:ident, $t:ty, $def:expr);+) => (
        $(
        impl MarkupData {
            #[inline]
            fn $method(&mut self) -> $t {
                util::replace(&mut self.$field, $def)
            }
        }
        )+
    )
)

gen_takes!(
    name       -> take_name, ~str, ~"";
    version    -> take_version, Option<common::XmlVersion>, None;
    encoding   -> take_encoding, ~str, ~"";
    standalone -> take_standalone, Option<bool>, None;
    value      -> take_value, ~str, ~"";
    ref_data   -> take_ref_data, ~str, ~"";
    quote      -> take_quote, Option<Token>, None;
    attributes -> take_attributes, ~[AttributeData], ~[]
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
        if self.finish_event.is_some() {
            return self.finish_event.get_ref().clone();
        }

        if self.next_event.is_some() {
            return util::replace(&mut self.next_event, None).unwrap();
        }

        for_each!(t in self.lexer.next_token(r) {
            match t {
                Ok(t) => match self.dispatch_token(t) {
                    Some(ev) => {
                        match ev {
                            events::EndDocument | events::Error(_) =>
                                self.finish_event = Some(ev.clone()),
                            _ => {}
                        }
                        return ev;
                    }
                    None => {}  // continue
                },

                // Pass through unexpected lexer errors
                Err(e) => {
                    let ev = events::Error(e.clone());
                    self.finish_event = Some(ev.clone());
                    return ev;
                }
            }
        })

        // Handle end of stream
        let ev = if self.depth == 0 {
            if self.encountered_element && self.st == OutsideTag {
                events::EndDocument
            } else if !self.encountered_element {
                self_error!("Unexpected end of stream: no root element found")
            } else {  // self.st != OutsideTag
                self_error!("Unexpected end of stream")  // TODO: add expected hint?
            }
        } else {
            self_error!("Unexpected end of stream: still inside the root element")
        };
        self.finish_event = Some(ev.clone());
        ev
    }

    fn error(&self, msg: ~str) -> XmlEvent {
        events::Error(Error::new(&self.lexer, msg))
    }

    fn dispatch_token(&mut self, t: Token) -> Option<XmlEvent> {
        match self.st.clone() {
            OutsideTag                     => self.outside_tag(t),
            InsideProcessingInstruction(s) => self.inside_processing_instruction(t, s),
            InsideDeclaration(s)           => self.inside_declaration(t, s),
            InsideOpeningTag(s)            => self.inside_opening_tag(t, s),
            InsideClosingTagName           => self.inside_closing_tag_name(t),
            InsideComment                  => self.inside_comment(t),
            InsideCData                    => self.inside_cdata(t),
            InsideReference(s)             => self.inside_reference(t, s.clone())
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
                self.into_state_continue(InsideReference(~OutsideTag)),

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
                        // We need to disable lexing errors inside CDATA
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
                       
                    // Don't need to check for declaration because it has mandatory attributes
                    // but there is none
                    match name.as_slice() {
                        // Name is empty, it is an error
                        "" => Some(self_error!("Encountered processing instruction without name")),

                        // Found <?xml-like PI not at the beginning of a document,
                        // it is an error - see section 2.6 of XML 1.1 spec
                        "xml"|"xmL"|"xMl"|"xML"|"Xml"|"XmL"|"XMl"|"XML" =>
                            Some(self_error!("Invalid processing instruction: <?{}", name)),

                        // All is ok, emitting event
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
                        // We have not ever encountered an element and have not parsed XML declaration
                        "xml" if !self.encountered_element && !self.parsed_declaration =>
                            self.into_state_continue(InsideDeclaration(BeforeVersion)),

                        // Found <?xml-like PI after the beginning of a document,
                        // it is an error - see section 2.6 of XML 1.1 spec
                        "xml"|"xmL"|"xMl"|"xML"|"Xml"|"XmL"|"XMl"|"XML"
                            if self.encountered_element || self.parsed_declaration =>
                            Some(self_error!("Invalid processing instruction: <?{}", name)),

                        // All is ok, starting parsing PI data
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

    /// Represents token dispatcher which parses attribute value.
    ///
    /// # Parameters
    /// * `t`        --- next token;
    /// * `on_value` --- a callback which is called when terminating quote is encountered
    fn expect_attribute_value(&mut self, t: Token, on_value: |&mut PullParser, ~str| -> Option<XmlEvent>) -> Option<XmlEvent> {
        match t {
            Whitespace(_) if self.data.quote.is_none() => None,  // skip leading whitespace

            DoubleQuote | SingleQuote => match self.data.quote.clone() {
                None => {  // Entered attribute value
                    self.data.quote = Some(t);
                    None
                }
                Some(ref q) if *q == t => {
                    self.data.quote = None;
                    let buf = self.take_buf();
                    on_value(self, buf)
                }
                _ => {
                    self.buf.push_str(t.to_str());
                    None
                }
            },

            _ if t.contains_char_data() => {
                self.buf.push_str(t.to_str());
                None
            }

            ReferenceStart => {
                let st = ~self.st.clone();
                self.into_state_continue(InsideReference(st))
            }

            _ => Some(self_error!("Unexpected token inside attribute value: {}", t.to_str()))
        }
    }

    // TODO: remove redundancy via macros or extra methods
    fn inside_declaration(&mut self, t: Token, s: DeclarationSubstate) -> Option<XmlEvent> {
        macro_rules! unexpected_token(($t:expr) => (Some(self_error!("Unexpected token inside XML declaration: {}", $t))))
        macro_rules! emit_start_document(
            ($encoding:expr, $version:expr) => ({
                let version = self.data.take_version();
                let encoding = $encoding;
                let standalone = $version;
                self.into_state_emit(OutsideTag, events::StartDocument {
                    version: version.unwrap_or(common::VERSION_1_0),
                    encoding: encoding.unwrap_or(~"UTF-8"),
                    standalone: standalone
                })
            })
        )
        match s {
            BeforeVersion => match t {
                Whitespace(_) => None,  // continue
                Character('v') => self.into_state_continue(InsideDeclaration(InsideVersion)),
                _ => unexpected_token!(t.to_str())
            },

            InsideVersion | AfterVersion => match t {
                Character(c) if is_name_char(c) && s == InsideVersion => {
                    self.buf.push_char(c);
                    None
                }
                Whitespace(_) if s == InsideVersion => self.into_state_continue(InsideDeclaration(AfterVersion)),
                Whitespace(_) if s == AfterVersion => None,
                EqualsSign => {
                    let buf = self.take_buf();
                    match buf.as_slice() {
                        "ersion" => self.into_state_continue(InsideDeclaration(InsideVersionValue)),
                        name => unexpected_token!(name)
                    }
                }
                _ => unexpected_token!(t.to_str())
            },

            InsideVersionValue => self.expect_attribute_value(t, |this, value| {
                match value.as_slice() {
                    "1.0" | "1.1" => {
                        this.data.version = Some(match value.as_slice() {
                            "1.0" => common::VERSION_1_0,
                            "1.1" => common::VERSION_1_1,
                            _     => unreachable!()
                        });
                        this.into_state_continue(InsideDeclaration(AfterVersionValue))
                    }
                    _ => Some(this.error(format!("Invalid XML version value: {}", value)))
                }
            }),

            AfterVersionValue => match t {
                Whitespace(_) => None,  // skip whitespace
                Character('e') => self.into_state_continue(InsideDeclaration(InsideEncoding)),
                Character('s') => self.into_state_continue(InsideDeclaration(InsideStandaloneDecl)),
                ProcessingInstructionEnd => emit_start_document!(None, None),
                _ => unexpected_token!(t.to_str())
            },

            InsideEncoding | AfterEncoding => match t {
                Character(c) if is_name_char(c) && s == InsideEncoding => {
                    self.buf.push_char(c);
                    None
                }
                Whitespace(_) if s == InsideEncoding => self.into_state_continue(InsideDeclaration(AfterEncoding)),
                Whitespace(_) if s == AfterEncoding => None,
                EqualsSign => {
                    let buf = self.take_buf();
                    match buf.as_slice() {
                        "ncoding" => self.into_state_continue(InsideDeclaration(InsideEncodingValue)),
                        name => unexpected_token!(name)
                    }
                }
                _ => unexpected_token!(t.to_str())
            },

            InsideEncodingValue => self.expect_attribute_value(t, |this, value| {
                this.data.encoding = value;
                this.into_state_continue(InsideDeclaration(BeforeStandaloneDecl))
            }),

            BeforeStandaloneDecl => match t {
                Whitespace(_) => None,  // skip whitespace
                Character('s') => self.into_state_continue(InsideDeclaration(InsideStandaloneDecl)),
                ProcessingInstructionEnd => emit_start_document!(Some(self.data.take_encoding()), None),
                _ => unexpected_token!(t.to_str())
            },

            InsideStandaloneDecl | AfterStandaloneDecl => match t {
                Character(c) if is_name_char(c) && s == InsideStandaloneDecl => {
                    self.buf.push_char(c);
                    None
                }
                Whitespace(_) if s == InsideStandaloneDecl => self.into_state_continue(InsideDeclaration(AfterStandaloneDecl)),
                Whitespace(_) if s == AfterStandaloneDecl => None,
                EqualsSign => {
                    let buf = self.take_buf();
                    match buf.as_slice() {
                        "standalone" => self.into_state_continue(InsideDeclaration(InsideStandaloneDeclValue)),
                        name => unexpected_token!(name)
                    }
                }
                _ => unexpected_token!(t.to_str())
            },

            InsideStandaloneDeclValue => self.expect_attribute_value(t, |this, value| {
                let standalone = match value.as_slice() {
                    "yes" => Some(true),
                    "no"  => Some(false),
                    _     => None
                };
                this.data.standalone = standalone;
                match standalone {
                    Some(standalone) => this.into_state_continue(InsideDeclaration(AfterStandaloneDeclValue)),
                    None => Some(this.error(format!("Invalid standalone declaration value: {}", value)))
                }
            }),

            AfterStandaloneDeclValue => match t {
                Whitespace(_) => None,  // skip whitespace
                ProcessingInstructionEnd => emit_start_document!(Some(self.data.take_encoding()), self.data.take_standalone()),
                _ => unexpected_token!(t.to_str())
            }
        }
    }

    fn inside_opening_tag(&mut self, t: Token, s: ElementSubstate) -> Option<XmlEvent> {
        macro_rules! unexpected_token(($t:expr) => (Some(self_error!("Unexpected token inside opening tag: {}", $t))))
        match s {
            InsideName => match t {
                Character(c) if (!self.buf_has_data() && is_name_start_char(c)) || 
                                (self.buf_has_data() && is_name_char(c))  => {
                    self.buf.push_char(c);
                    None
                }
                Whitespace(_) => self.into_state_continue(InsideOpeningTag(InsideTag)),
                TagEnd => {
                    let name = self.take_buf();
                    self.into_state_emit(OutsideTag, events::StartElement {
                        name: common::parse_name(name),  // TODO: pass namespace map
                        attributes: ~[]
                    })
                }
                EmptyTagEnd => {
                    let name = self.take_buf();
                    let qname = common::parse_name(name);  // TODO: pass namespace map
                    self.next_event = Some(events::EndElement{
                        name: qname.clone()
                    });
                    self.into_state_emit(OutsideTag, events::StartElement {
                        name: qname,
                        attributes: ~[]
                    })
                }
                _ => unexpected_token!(t.to_str())
            },

            InsideTag => match t {
                Whitespace(_) => None,  // skip whitespace
                Character(c) if is_name_start_char(c) => {
                    self.buf.push_char(c);
                    self.into_state_continue(InsideOpeningTag(InsideAttributeName))
                }
                _ => unexpected_token!(t.to_str())
            },

            InsideAttributeName | AfterAttributeName => match t {
                Character(c) if s == InsideAttributeName && is_name_char(c) => {
                    self.buf.push_char(c);
                    None
                }
                Whitespace(_) if s == InsideAttributeName => self.into_state_continue(InsideOpeningTag(AfterAttributeName)),
                Whitespace(_) if s == AfterAttributeName => None,
                EqualsSign => {
                    self.data.name = self.take_buf();
                    self.into_state_continue(InsideOpeningTag(InsideAttributeValue))
                }
                _ => unexpected_token!(t.to_str())
            },

            InsideAttributeValue => self.expect_attribute_value(t, |this, value| {
                let name = this.data.take_name();
                this.data.attributes.push(AttributeData {
                    name: common::parse_name(name),
                    value: value
                });
                this.into_state_continue(InsideOpeningTag(InsideTag))
            })
        }
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

    fn inside_reference(&mut self, t: Token, prev_st: ~State) -> Option<XmlEvent> {
        None
    }
}

#[cfg(test)]
mod tests {
    use std::io::mem::MemReader;

    use super::PullParser;
}
