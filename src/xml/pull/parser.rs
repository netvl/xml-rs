use std::util;

use common;
use common::{Error, XmlVersion, Name, is_name_start_char, is_name_char};
use events;
use events::XmlEvent;

use pull::config::ParserConfig;
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
    priv config: ParserConfig,
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

pub fn new(config: ParserConfig) -> PullParser {
    PullParser {
        config: config,
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
    InsideOpeningTag(OpeningTagSubstate),
    InsideClosingTag(ClosingTagSubstate),
    InsideProcessingInstruction(ProcessingInstructionSubstate),
    InsideComment,
    InsideCData,
    InsideDeclaration(DeclarationSubstate),
    InsideReference(~State, ReferenceDestination)
}

#[deriving(Clone, Eq)]
enum ReferenceDestination {
    BufDestination,
    DataValueDestination
}

#[deriving(Clone, Eq)]
enum OpeningTagSubstate {
    InsideName,

    InsideTag,

    InsideAttributeName,
    AfterAttributeName,

    InsideAttributeValue,
}

#[deriving(Clone, Eq)]
enum ClosingTagSubstate {
    CTInsideName,
    CTAfterName
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

impl AttributeData {
    fn into_attribute(self) -> common::Attribute {
        common::Attribute {   // TODO: do something with clone()
            value: self.value.clone(),
            name: self.name
        }
    }
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
                    let ev = events::Error(e);
                    self.finish_event = Some(ev.clone());
                    return ev;
                }
            }
        })

        // Handle end of stream
        let ev = if self.depth == 0 {
            if self.encountered_element && self.st == OutsideTag {  // all is ok
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

    #[inline]
    fn error(&self, msg: ~str) -> XmlEvent {
        events::Error(Error::new(&self.lexer, msg))
    }

    fn dispatch_token(&mut self, t: Token) -> Option<XmlEvent> {
        match self.st.clone() {
            OutsideTag                     => self.outside_tag(t),
            InsideProcessingInstruction(s) => self.inside_processing_instruction(t, s),
            InsideDeclaration(s)           => self.inside_declaration(t, s),
            InsideOpeningTag(s)            => self.inside_opening_tag(t, s),
            InsideClosingTag(s)            => self.inside_closing_tag_name(t, s),
            InsideComment                  => self.inside_comment(t),
            InsideCData                    => self.inside_cdata(t),
            InsideReference(s, dest)       => self.inside_reference(t, *s, dest)
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
    fn append_char_continue(&mut self, c: char) -> Option<XmlEvent> {
        self.buf.push_char(c);
        None
    }

    #[inline]
    fn append_str_continue(&mut self, s: &str) -> Option<XmlEvent> {
        self.buf.push_str(s);
        None
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
                self.into_state_continue(InsideReference(~OutsideTag, BufDestination)),

            Whitespace(_) if self.depth == 0 => None,  // skip whitespace outside of the root element

            _ if t.contains_char_data() && self.depth == 0 =>
                Some(self_error!("Unexpected characters outside the root element: {}", t.to_str())),

            Whitespace(c) => self.append_char_continue(c),

            _ if t.contains_char_data() => {  // Non-whitespace char data
                self.inside_whitespace = false;
                self.append_str_continue(t.to_str())
            }

            _ => {  // Encountered some meaningful event, flush the buffer as an event
                let mut next_event = if self.buf_has_data() {
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

                    OpeningTagStart => {
                        // If declaration was not parsed and we have encountered an element,
                        // emit this declaration as the next event.
                        if !self.parsed_declaration {
                            let sd_event = events::StartDocument {
                                version: common::Version10,
                                encoding: ~"UTF-8",
                                standalone: None
                            };
                            if next_event.is_none() {
                                next_event = Some(sd_event);
                            } else {
                                self.next_event = Some(sd_event);
                            }
                        }
                        self.encountered_element = true;
                        self.depth += 1;
                        self.into_state(InsideOpeningTag(InsideName), next_event)
                    }

                    ClosingTagStart => {
                        self.depth -= 1;
                        self.into_state(InsideClosingTag(CTInsideName), next_event)
                    }

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
                Character(c) if !self.buf_has_data() && is_name_start_char(c) ||
                                 self.buf_has_data() && is_name_char(c) => self.append_char_continue(c),

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

                Whitespace(_) => {
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
                    let value = self.data.take_value();
                    on_value(self, value)
                }
                _ => {
                    self.buf.push_str(t.to_str());
                    None
                }
            },

            _ if t.contains_char_data() => {
                self.data.value.push_str(t.to_str());
                None
            }

            ReferenceStart => {
                let st = ~self.st.clone();
                self.into_state_continue(InsideReference(st, DataValueDestination))
            }

            _ => Some(self_error!("Unexpected token inside attribute value: {}", t.to_str()))
        }
    }

    // TODO: remove redundancy via macros or extra methods
    fn inside_declaration(&mut self, t: Token, s: DeclarationSubstate) -> Option<XmlEvent> {
        macro_rules! unexpected_token(($t:expr) => (Some(self_error!("Unexpected token inside XML declaration: {}", $t))))
        macro_rules! emit_start_document(
            ($encoding:expr, $standalone:expr) => ({
                let version = self.data.take_version();
                let encoding = $encoding;
                let standalone = $standalone;
                self.into_state_emit(OutsideTag, events::StartDocument {
                    version: version.unwrap_or(common::Version10),
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
                            "1.0" => common::Version10,
                            "1.1" => common::Version11,
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
                        "tandalone" => self.into_state_continue(InsideDeclaration(InsideStandaloneDeclValue)),
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
                if standalone.is_some() {
                    this.data.standalone = standalone;
                    this.into_state_continue(InsideDeclaration(AfterStandaloneDeclValue))
                } else {
                    Some(this.error(format!("Invalid standalone declaration value: {}", value)))
                }
            }),

            AfterStandaloneDeclValue => match t {
                Whitespace(_) => None,  // skip whitespace
                ProcessingInstructionEnd => emit_start_document!(Some(self.data.take_encoding()), self.data.take_standalone()),
                _ => unexpected_token!(t.to_str())
            }
        }
    }

    fn emit_start_element(&mut self, emit_end_element: bool) -> Option<XmlEvent> {
        let name = self.take_buf();
        let qname = match common::parse_name(name) {
            Ok(qname) => qname,
            Err(e) => return Some(self_error!("Error parsing qualified name {}: {}", name, e.to_str()))
        };
        let attributes = self.data.take_attributes();
        if emit_end_element {
            self.depth -= 1;
            self.next_event = Some(events::EndElement {
                name: qname.clone()
            });
        }
        self.into_state_emit(OutsideTag, events::StartElement {
            name: qname,  // TODO: pass namespace map
            attributes: attributes.move_iter().map(|a| a.into_attribute()).collect()
        })
    }

    fn inside_opening_tag(&mut self, t: Token, s: OpeningTagSubstate) -> Option<XmlEvent> {
        macro_rules! unexpected_token(($t:expr) => (Some(self_error!("Unexpected token inside opening tag: {}", $t))))
        match s {
            InsideName => match t {
                Character(c) if !self.buf_has_data() && is_name_start_char(c) || 
                                 self.buf_has_data() && is_name_char(c) => self.append_char_continue(c),
                Whitespace(_) => self.into_state_continue(InsideOpeningTag(InsideTag)),
                TagEnd => self.emit_start_element(false),
                EmptyTagEnd => self.emit_start_element(true),
                _ => unexpected_token!(t.to_str())
            },

            InsideTag => match t {
                Whitespace(_) => None,  // skip whitespace
                Character(c) if is_name_start_char(c) => {
                    self.data.name.push_char(c);
                    self.into_state_continue(InsideOpeningTag(InsideAttributeName))
                }
                TagEnd => self.emit_start_element(false),
                EmptyTagEnd => self.emit_start_element(true),
                _ => unexpected_token!(t.to_str())
            },

            InsideAttributeName | AfterAttributeName => match t {
                Character(c) if s == InsideAttributeName && is_name_char(c) => {
                    self.data.name.push_char(c);
                    None
                }
                Whitespace(_) if s == InsideAttributeName => self.into_state_continue(InsideOpeningTag(AfterAttributeName)),
                Whitespace(_) if s == AfterAttributeName => None,
                EqualsSign => self.into_state_continue(InsideOpeningTag(InsideAttributeValue)),
                _ => unexpected_token!(t.to_str())
            },

            InsideAttributeValue => self.expect_attribute_value(t, |this, value| {
                let name = this.data.take_name();
                let qname = match common::parse_name(name) {
                    Ok(qname) => qname,
                    Err(e) => return Some(this.error(format!("Error parsing qualified name {}: {}", name, e.to_str())))
                };
                this.data.attributes.push(AttributeData {
                    name: qname,
                    value: value
                });
                this.into_state_continue(InsideOpeningTag(InsideTag))
            })
        }
    }

    fn inside_closing_tag_name(&mut self, t: Token, s: ClosingTagSubstate) -> Option<XmlEvent> {
        match s {
            CTInsideName => match t {
                Character(c) if !self.buf_has_data() && is_name_start_char(c) || 
                                 self.buf_has_data() && is_name_char(c) =>
                    self.append_char_continue(c),
                Whitespace(_) => self.into_state_continue(InsideClosingTag(CTAfterName)),
                TagEnd => {
                    let name = self.take_buf();
                    if name.is_empty() {
                        Some(self_error!("Encountered closing tag without name"))
                    } else {
                        let qname = match common::parse_name(name) {
                            Ok(qname) => qname,
                            Err(e) => return Some(self_error!("Error parsing qualified name {}: {}", name, e.to_str()))
                        };
                        self.into_state_emit(OutsideTag, events::EndElement { name: qname })
                    }
                }
                _ => Some(self_error!("Unexpected token inside closing tag: {}", t.to_str()))
            },
            CTAfterName => match t {
                Whitespace(_) => None,  //  Skip whitespace
                TagEnd => {
                    let name = self.take_buf();
                    if name.is_empty() {
                        Some(self_error!("Encountered closing tag without name"))
                    } else {
                        let qname = match common::parse_name(name) {
                            Ok(qname) => qname,
                            Err(e) => return Some(self_error!("Error parsing qualified name {}: {}", name, e.to_str()))
                        };
                        self.into_state_emit(OutsideTag, events::EndElement { name: qname })
                    }
                }
                _ => Some(self_error!("Unexpected token inside closing tag: {}", t.to_str()))
            }
        }
    }

    fn inside_comment(&mut self, t: Token) -> Option<XmlEvent> {
        match t {
            // Double dash is illegal inside a comment
            Chunk(~"--") => Some(self_error!("Unexpected token inside a comment: --")),

            CommentEnd => {
                self.lexer.enable_errors();
                let data = self.take_buf();
                self.into_state_emit(OutsideTag, events::Comment(data))
            }

            _ => self.append_str_continue(t.to_str()),
        }
    }

    fn inside_cdata(&mut self, t: Token) -> Option<XmlEvent> {
        match t {
            CDataEnd => {
                self.lexer.enable_errors();
                let data = self.take_buf();
                self.into_state_emit(OutsideTag, events::CData(data))
            }

            _ => self.append_str_continue(t.to_str())
        }
    }

    fn inside_reference(&mut self, t: Token, prev_st: State, dest: ReferenceDestination) -> Option<XmlEvent> {
        use std::char;
        use std::num::from_str_radix;

        match t {
            Character(c) if !self.data.ref_data.is_empty() && is_name_char(c) || 
                             self.data.ref_data.is_empty() && (is_name_start_char(c) || c == '#') => {
                self.data.ref_data.push_char(c);
                None
            }

            ReferenceEnd => {
                let name = self.data.take_ref_data();
                let name_len = name.char_len();  // compute once
                let c = match name.as_slice() {
                    "lt"   => Ok('<'),
                    "gt"   => Ok('>'),
                    "amp"  => Ok('&'),
                    "apos" => Ok('\''),
                    "quot" => Ok('"'),
                    ""     => Err(self_error!("Encountered empty entity")),
                    _ if name_len > 2 && name.slice_chars(0, 2) == "#x" => {
                        let num_str = name.slice_chars(2, name_len);
                        if num_str == "0" {
                            Err(self_error!("Null character entity is not allowed"))
                        } else {
                            match from_str_radix(num_str, 16).and_then(char::from_u32) {
                                Some(c) => Ok(c),
                                None    => Err(self_error!("Invalid hexadecimal character number in an entity: {}", name))
                            }
                        }
                    }
                    _ if name_len > 1 && name.char_at(0) == '#' => {
                        let num_str = name.slice_chars(1, name_len);
                        if num_str == "0" {
                            Err(self_error!("Null character entity is not allowed"))
                        } else {
                            match from_str_radix(num_str, 10).and_then(char::from_u32) {
                                Some(c) => Ok(c),
                                None    => Err(self_error!("Invalid decimal character number in an entity: {}", name))
                            }
                        }
                    },
                    _ => Err(self_error!("Unexpected entity: {}", name))
                };
                match c {
                    Ok(c) => {
                        match dest {
                            BufDestination => &mut self.buf,
                            DataValueDestination => &mut self.data.value,
                        }.push_char(c);
                        self.into_state_continue(prev_st)
                    }
                    Err(e) => Some(e)
                }
            }

            _ => Some(self_error!("Unexpected token inside an entity: {}", t.to_str()))
        }
    }
}

#[cfg(test)]
mod tests {
}
