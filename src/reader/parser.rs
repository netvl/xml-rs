//! Contains an implementation of pull-based XML parser.

use std::mem;

use common;
use common::{Error, XmlVersion, is_name_start_char, is_name_char, is_whitespace_char};
use name::OwnedName;
use attribute::OwnedAttribute;
use namespace;
use namespace::{NamespaceStack};

use reader::events::XmlEvent;
use reader::config::ParserConfig;
use reader::lexer;
use reader::lexer::{PullLexer, Token};

static DEFAULT_VERSION: XmlVersion      = XmlVersion::Version10;
static DEFAULT_ENCODING: &'static str   = "UTF-8";
static DEFAULT_STANDALONE: Option<bool> = None;

type ElementStack = Vec<OwnedName>;

/// Pull-based XML parser.
pub struct PullParser {
    config: ParserConfig,
    lexer: PullLexer,
    st: State,
    buf: String,
    nst: NamespaceStack,

    data: MarkupData,
    finish_event: Option<XmlEvent>,
    next_event: Option<XmlEvent>,
    est: ElementStack,

    encountered_element: bool,
    parsed_declaration: bool,
    inside_whitespace: bool,
    read_prefix_separator: bool,
    pop_namespace: bool
}

impl PullParser {
    /// Returns a new parser using the given config.
    pub fn new(config: ParserConfig) -> PullParser {
        PullParser {
            config: config,
            lexer: lexer::new(),
            st: State::OutsideTag,
            buf: String::new(),
            nst: NamespaceStack::default(),

            data: MarkupData {
                name: String::new(),
                version: None,
                encoding: None,
                standalone: None,
                ref_data: String::new(),
                element_name: None,
                quote: None,
                attr_name: None,
                attributes: vec!()
            },
            finish_event: None,
            next_event: None,
            est: Vec::new(),

            encountered_element: false,
            parsed_declaration: false,
            inside_whitespace: true,
            read_prefix_separator: false,
            pop_namespace: false
        }
    }
}

#[derive(Clone, PartialEq)]
enum State {
    OutsideTag,
    InsideOpeningTag(OpeningTagSubstate),
    InsideClosingTag(ClosingTagSubstate),
    InsideProcessingInstruction(ProcessingInstructionSubstate),
    InsideComment,
    InsideCData,
    InsideDeclaration(DeclarationSubstate),
    InsideDoctype,
    InsideReference(Box<State>)
}

#[derive(Clone, PartialEq)]
enum OpeningTagSubstate {
    InsideName,

    InsideTag,

    InsideAttributeName,
    AfterAttributeName,

    InsideAttributeValue,
}

#[derive(Clone, PartialEq)]
enum ClosingTagSubstate {
    CTInsideName,
    CTAfterName
}

#[derive(Clone, PartialEq)]
enum ProcessingInstructionSubstate {
    PIInsideName,
    PIInsideData
}

#[derive(Clone, PartialEq)]
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

#[derive(PartialEq)]
enum QualifiedNameTarget {
    AttributeNameTarget,
    OpeningTagNameTarget,
    ClosingTagNameTarget
}

#[derive(Copy, PartialEq, Eq)]
enum QuoteToken {
    SingleQuoteToken,
    DoubleQuoteToken
}

impl QuoteToken {
    fn from_token(t: &Token) -> QuoteToken {
        match *t {
            Token::SingleQuote => QuoteToken::SingleQuoteToken,
            Token::DoubleQuote => QuoteToken::DoubleQuoteToken,
            _ => panic!("Unexpected token: {}", t.to_string())
        }
    }

    fn as_token(self) -> Token {
        match self {
            QuoteToken::SingleQuoteToken => Token::SingleQuote,
            QuoteToken::DoubleQuoteToken => Token::DoubleQuote
        }
    }
}

// TODO: remove? seems to be superseded by OwnedAttribute completely
struct AttributeData {
    name: OwnedName,
    value: String
}

impl AttributeData {
    fn into_attribute(self) -> OwnedAttribute {
        let AttributeData { name, value } = self;
        OwnedAttribute::new(name, value)
    }
}

struct MarkupData {
    name: String,     // used for processing instruction name
    ref_data: String,  // used for reference content

    version: Option<common::XmlVersion>,  // used for XML declaration version
    encoding: Option<String>,  // used for XML declaration encoding
    standalone: Option<bool>,  // used for XML declaration standalone parameter

    element_name: Option<OwnedName>,  // used for element name

    quote: Option<QuoteToken>,  // used to hold opening quote for attribute value
    attr_name: Option<OwnedName>,  // used to hold attribute name
    attributes: Vec<AttributeData>   // used to hold all accumulated attributes
}

macro_rules! gen_takes(
    ($($field:ident -> $method:ident, $t:ty, $def:expr);+) => (
        $(
        impl MarkupData {
            #[inline]
            fn $method(&mut self) -> $t {
                mem::replace(&mut self.$field, $def)
            }
        }
        )+
    )
);

gen_takes!(
    name         -> take_name, String, String::new();
    ref_data     -> take_ref_data, String, String::new();

    version      -> take_version, Option<common::XmlVersion>, None;
    encoding     -> take_encoding, Option<String>, None;
    standalone   -> take_standalone, Option<bool>, None;

    element_name -> take_element_name, Option<OwnedName>, None;

    attr_name    -> take_attr_name, Option<OwnedName>, None;
    attributes   -> take_attributes, Vec<AttributeData>, vec!()
);

macro_rules! self_error(
    ($this:ident; $msg:expr) => ($this.error($msg.to_string()));
    ($this:ident; $fmt:expr, $($arg:expr),+) => ($this.error(format!($fmt, $($arg),+)))
);

impl PullParser {
    /// Returns next event read from the given buffer.
    ///
    /// This method should be always called with the same buffer. If you call it
    /// providing different buffers each time, the result will be undefined.
    pub fn next<B: Buffer>(&mut self, r: &mut B) -> XmlEvent {
        if self.finish_event.is_some() {
            return self.finish_event.as_ref().unwrap().clone();
        }

        if self.next_event.is_some() {
            return mem::replace(&mut self.next_event, None).unwrap();
        }

        if self.pop_namespace {
            self.pop_namespace = false;
            self.nst.pop();
        }

        for_each!(t in self.lexer.next_token(r) ; {
            match t {
                Ok(t) => match self.dispatch_token(t) {
                    Some(ev) => {
                        match ev {
                            XmlEvent::EndDocument | XmlEvent::Error(_) =>
                                self.finish_event = Some(ev.clone()),
                            _ => {}
                        }
                        return ev;
                    }
                    None => {}  // continue
                },

                // Pass through unexpected lexer errors
                Err(e) => {
                    let ev = XmlEvent::Error(e);
                    self.finish_event = Some(ev.clone());
                    return ev;
                }
            }
        });

        // Handle end of stream
        let ev = if self.depth() == 0 {
            if self.encountered_element && self.st == State::OutsideTag {  // all is ok
                XmlEvent::EndDocument
            } else if !self.encountered_element {
                self_error!(self; "Unexpected end of stream: no root element found")
            } else {  // self.st != State::OutsideTag
                self_error!(self; "Unexpected end of stream")  // TODO: add expected hint?
            }
        } else {
            self_error!(self; "Unexpected end of stream: still inside the root element")
        };
        self.finish_event = Some(ev.clone());
        ev
    }

    #[inline]
    fn error(&self, msg: String) -> XmlEvent {
        XmlEvent::Error(Error::new(&self.lexer, msg))
    }

    fn dispatch_token(&mut self, t: Token) -> Option<XmlEvent> {
        match self.st.clone() {
            State::OutsideTag                     => self.outside_tag(t),
            State::InsideProcessingInstruction(s) => self.inside_processing_instruction(t, s),
            State::InsideDeclaration(s)           => self.inside_declaration(t, s),
            State::InsideDoctype                  => self.inside_doctype(t),
            State::InsideOpeningTag(s)            => self.inside_opening_tag(t, s),
            State::InsideClosingTag(s)            => self.inside_closing_tag_name(t, s),
            State::InsideComment                  => self.inside_comment(t),
            State::InsideCData                    => self.inside_cdata(t),
            State::InsideReference(s)             => self.inside_reference(t, *s)
        }
    }

    #[inline]
    fn depth(&self) -> usize {
        self.est.len()
    }

    #[inline]
    fn buf_has_data(&self) -> bool {
        self.buf.len() > 0
    }

    #[inline]
    fn take_buf(&mut self) -> String {
        mem::replace(&mut self.buf, String::new())
    }

    #[inline]
    fn append_char_continue(&mut self, c: char) -> Option<XmlEvent> {
        self.buf.push(c);
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

    /// Dispatches tokens in order to process qualified name. If qualified name cannot be parsed,
    /// an error is returned.
    ///
    /// # Parameters
    /// * `t`       --- next token;
    /// * `on_name` --- a callback which is executed when whitespace is encountered.
    fn read_qualified_name<F>(&mut self, t: Token, target: QualifiedNameTarget, on_name: F) -> Option<XmlEvent>
      where F: Fn(&mut PullParser, Token, OwnedName) -> Option<XmlEvent> {
        // We can get here for the first time only when self.data.name contains zero or one character,
        // but first character cannot be a colon anyway
        if self.buf.len() <= 1 {
            self.read_prefix_separator = false;
        }

        let invoke_callback = |&: this: &mut PullParser, t| {
            let name = this.take_buf();
            match name.as_slice().parse() {
                Some(name) => on_name(this, t, name),
                None => Some(self_error!(this; "Qualified name is invalid: {}", name))
            }
        };

        match t {
            // There can be only one colon, and not as the first character
            Token::Character(':') if self.buf_has_data() && !self.read_prefix_separator => {
                self.buf.push(':');
                self.read_prefix_separator = true;
                None
            }

            Token::Character(c) if c != ':' && (!self.buf_has_data() && is_name_start_char(c) ||
                                          self.buf_has_data() && is_name_char(c)) =>
                self.append_char_continue(c),

            Token::EqualsSign if target == QualifiedNameTarget::AttributeNameTarget => invoke_callback(self, t),

            Token::EmptyTagEnd if target == QualifiedNameTarget::OpeningTagNameTarget => invoke_callback(self, t),

            Token::TagEnd if target == QualifiedNameTarget::OpeningTagNameTarget ||
                      target == QualifiedNameTarget::ClosingTagNameTarget => invoke_callback(self, t),

            Token::Whitespace(_) => invoke_callback(self, t),

            _ => Some(self_error!(self; "Unexpected token inside qualified name: {}", t.to_string()))
        }
    }

    /// Dispatches tokens in order to process attribute value.
    ///
    /// # Parameters
    /// * `t`        --- next token;
    /// * `on_value` --- a callback which is called when terminating quote is encountered.
    fn read_attribute_value<F>(&mut self, t: Token, on_value: F) -> Option<XmlEvent>
      where F: Fn(&mut PullParser, String) -> Option<XmlEvent> {
        match t {
            Token::Whitespace(_) if self.data.quote.is_none() => None,  // skip leading whitespace

            Token::DoubleQuote | Token::SingleQuote => match self.data.quote {
                None => {  // Entered attribute value
                    self.data.quote = Some(QuoteToken::from_token(&t));
                    None
                }
                Some(q) if q.as_token() == t => {
                    self.data.quote = None;
                    let value = self.take_buf();
                    on_value(self, value)
                }
                _ => self.append_str_continue(t.to_string().as_slice()),
            },

            Token::ReferenceStart => {
                let st = Box::new(self.st.clone());
                self.into_state_continue(State::InsideReference(st))
            }

            Token::OpeningTagStart =>
                Some(self_error!(self; "Unexpected token inside attribute value: <")),

            // Every character except " and ' and < is okay
            _  => self.append_str_continue(t.to_string().as_slice()),
        }
    }

    fn outside_tag(&mut self, t: Token) -> Option<XmlEvent> {
        match t {
            Token::ReferenceStart =>
                self.into_state_continue(State::InsideReference(Box::new(State::OutsideTag))),

            Token::Whitespace(_) if self.depth() == 0 => None,  // skip whitespace outside of the root element

            _ if t.contains_char_data() && self.depth() == 0 =>
                Some(self_error!(self; "Unexpected characters outside the root element: {}", t.to_string())),

            Token::Whitespace(c) => self.append_char_continue(c),

            _ if t.contains_char_data() => {  // Non-whitespace char data
                self.inside_whitespace = false;
                self.append_str_continue(t.to_string().as_slice())
            }

            Token::ReferenceEnd => { // Semi-colon in a text outside an entity
                self.inside_whitespace = false;
                self.append_str_continue(Token::ReferenceEnd.as_static_str().unwrap())
            }

            Token::CommentStart if self.config.coalesce_characters && self.config.ignore_comments => {
                // We need to disable lexing errors inside comments
                self.lexer.disable_errors();
                self.into_state_continue(State::InsideComment)
            }

            Token::CDataStart if self.config.coalesce_characters && self.config.cdata_to_characters => {
                // We need to disable lexing errors inside CDATA
                self.lexer.disable_errors();
                self.into_state_continue(State::InsideCData)
            }

            _ => {
                // Encountered some markup event, flush the buffer as characters
                // or a whitespace
                let mut next_event = if self.buf_has_data() {
                    let buf = self.take_buf();
                    if self.inside_whitespace && self.config.trim_whitespace {
                        None
                    } else if self.inside_whitespace && !self.config.whitespace_to_characters {
                        Some(XmlEvent::Whitespace(buf))
                    } else if self.config.trim_whitespace {
                        Some(XmlEvent::Characters(buf.as_slice().trim_matches(is_whitespace_char).to_string()))
                    } else {
                        Some(XmlEvent::Characters(buf))
                    }
                } else { None };
                self.inside_whitespace = true;  // Reset inside_whitespace flag
                match t {
                    Token::ProcessingInstructionStart =>
                        self.into_state(State::InsideProcessingInstruction(ProcessingInstructionSubstate::PIInsideName), next_event),

                    Token::DoctypeStart if !self.encountered_element => {
                        self.lexer.disable_errors();
                        self.into_state(State::InsideDoctype, next_event)
                    }

                    Token::OpeningTagStart => {
                        // If declaration was not parsed and we have encountered an element,
                        // emit this declaration as the next event.
                        if !self.parsed_declaration {
                            self.parsed_declaration = true;
                            let sd_event = XmlEvent::StartDocument {
                                version: DEFAULT_VERSION,
                                encoding: DEFAULT_ENCODING.to_string(),
                                standalone: DEFAULT_STANDALONE
                            };
                            // next_event is always none here because we're outside of
                            // the root element
                            next_event = Some(sd_event);
                        }
                        self.encountered_element = true;
                        self.nst.push_empty();
                        self.into_state(State::InsideOpeningTag(OpeningTagSubstate::InsideName), next_event)
                    }

                    Token::ClosingTagStart if self.depth() > 0 =>
                        self.into_state(State::InsideClosingTag(ClosingTagSubstate::CTInsideName), next_event),

                    Token::CommentStart => {
                        // We need to disable lexing errors inside comments
                        self.lexer.disable_errors();
                        self.into_state(State::InsideComment, next_event)
                    }

                    Token::CDataStart => {
                        // We need to disable lexing errors inside CDATA
                        self.lexer.disable_errors();
                        self.into_state(State::InsideCData, next_event)
                    }

                    _ => Some(self_error!(self; "Unexpected token: {}", t.to_string()))
                }
            }
        }
    }

    fn inside_doctype(&mut self, t: Token) -> Option<XmlEvent> {
        match t {
            Token::TagEnd => {
                self.lexer.enable_errors();
                self.into_state_continue(State::OutsideTag)
            }

            _ => None
        }
    }

    fn inside_processing_instruction(&mut self, t: Token, s: ProcessingInstructionSubstate) -> Option<XmlEvent> {
        match s {
            ProcessingInstructionSubstate::PIInsideName => match t {
                Token::Character(c) if !self.buf_has_data() && is_name_start_char(c) ||
                                 self.buf_has_data() && is_name_char(c) => self.append_char_continue(c),

                Token::ProcessingInstructionEnd => {
                    // self.buf contains PI name
                    let name = self.take_buf();

                    // Don't need to check for declaration because it has mandatory attributes
                    // but there is none
                    match name.as_slice() {
                        // Name is empty, it is an error
                        "" => Some(self_error!(self; "Encountered processing instruction without name")),

                        // Found <?xml-like PI not at the beginning of a document,
                        // it is an error - see section 2.6 of XML 1.1 spec
                        "xml"|"xmL"|"xMl"|"xML"|"Xml"|"XmL"|"XMl"|"XML" =>
                            Some(self_error!(self; "Invalid processing instruction: <?{}", name)),

                        // All is ok, emitting event
                        _ => {
                            self.into_state_emit(
                                State::OutsideTag,
                                XmlEvent::ProcessingInstruction {
                                    name: name,
                                    data: None
                                }
                            )
                        }
                    }
                }

                Token::Whitespace(_) => {
                    // self.buf contains PI name
                    let name = self.take_buf();

                    match name.as_slice() {
                        // We have not ever encountered an element and have not parsed XML declaration
                        "xml" if !self.encountered_element && !self.parsed_declaration =>
                            self.into_state_continue(State::InsideDeclaration(DeclarationSubstate::BeforeVersion)),

                        // Found <?xml-like PI after the beginning of a document,
                        // it is an error - see section 2.6 of XML 1.1 spec
                        "xml"|"xmL"|"xMl"|"xML"|"Xml"|"XmL"|"XMl"|"XML"
                            if self.encountered_element || self.parsed_declaration =>
                            Some(self_error!(self; "Invalid processing instruction: <?{}", name)),

                        // All is ok, starting parsing PI data
                        _ => {
                            self.lexer.disable_errors();  // data is arbitrary, so disable errors
                            self.data.name = name;
                            self.into_state_continue(State::InsideProcessingInstruction(ProcessingInstructionSubstate::PIInsideData))
                        }

                    }
                }

                _ => Some(self_error!(self; "Unexpected token: <?{}{}", self.buf, t.to_string()))
            },

            ProcessingInstructionSubstate::PIInsideData => match t {
                Token::ProcessingInstructionEnd => {
                    self.lexer.enable_errors();
                    let name = self.data.take_name();
                    let data = self.take_buf();
                    self.into_state_emit(
                        State::OutsideTag,
                        XmlEvent::ProcessingInstruction {
                            name: name,
                            data: Some(data)
                        }
                    )
                },

                // Any other token should be treated as plain characters
                _ => {
                    self.buf.push_str(t.to_string().as_slice());
                    None
                }
            },
        }
    }

    // TODO: remove redundancy via macros or extra methods
    fn inside_declaration(&mut self, t: Token, s: DeclarationSubstate) -> Option<XmlEvent> {
        macro_rules! unexpected_token(
            ($this:expr; $t:expr) => (Some($this.error(format!("Unexpected token inside XML declaration: {}", $t.to_string()))));
            ($t:expr) => (unexpected_token!(self; $t));
        );

        #[inline]
        fn emit_start_document(this: &mut PullParser) -> Option<XmlEvent> {
            this.parsed_declaration = true;
            let version = this.data.take_version();
            let encoding = this.data.take_encoding();
            let standalone = this.data.take_standalone();
            this.into_state_emit(State::OutsideTag, XmlEvent::StartDocument {
                version: version.unwrap_or(DEFAULT_VERSION),
                encoding: encoding.unwrap_or(DEFAULT_ENCODING.to_string()),
                standalone: standalone
            })
        }

        match s {
            DeclarationSubstate::BeforeVersion => match t {
                Token::Whitespace(_) => None,  // continue
                Token::Character('v') => self.into_state_continue(State::InsideDeclaration(DeclarationSubstate::InsideVersion)),
                _ => unexpected_token!(t)
            },

            DeclarationSubstate::InsideVersion => self.read_qualified_name(t, QualifiedNameTarget::AttributeNameTarget, |this, token, name| {
                match name.local_name.as_slice() {
                    "ersion" if name.namespace.is_none() =>
                        this.into_state_continue(State::InsideDeclaration(
                            if token == Token::EqualsSign {
                                DeclarationSubstate::InsideVersionValue
                            } else {
                                DeclarationSubstate::AfterVersion
                            }
                        )),
                    _ => unexpected_token!(this; name)
                }
            }),

            DeclarationSubstate::AfterVersion => match t {
                Token::Whitespace(_) => None,
                Token::EqualsSign => self.into_state_continue(State::InsideDeclaration(DeclarationSubstate::InsideVersionValue)),
                _ => unexpected_token!(t)
            },

            DeclarationSubstate::InsideVersionValue => self.read_attribute_value(t, |this, value| {
                this.data.version = match value.as_slice() {
                    "1.0" => Some(XmlVersion::Version10),
                    "1.1" => Some(XmlVersion::Version11),
                    _     => None
                };
                if this.data.version.is_some() {
                    this.into_state_continue(State::InsideDeclaration(DeclarationSubstate::AfterVersionValue))
                } else {
                    Some(self_error!(this; "Unexpected XML version value: {}", value))
                }
            }),

            DeclarationSubstate::AfterVersionValue => match t {
                Token::Whitespace(_) => None,  // skip whitespace
                Token::Character('e') => self.into_state_continue(State::InsideDeclaration(DeclarationSubstate::InsideEncoding)),
                Token::Character('s') => self.into_state_continue(State::InsideDeclaration(DeclarationSubstate::InsideStandaloneDecl)),
                Token::ProcessingInstructionEnd => emit_start_document(self),
                _ => unexpected_token!(t)
            },

            DeclarationSubstate::InsideEncoding => self.read_qualified_name(t, QualifiedNameTarget::AttributeNameTarget, |this, token, name| {
                match name.local_name.as_slice() {
                    "ncoding" if name.namespace.is_none() =>
                        this.into_state_continue(State::InsideDeclaration(
                            if token == Token::EqualsSign { DeclarationSubstate::InsideEncodingValue } else { DeclarationSubstate::AfterEncoding }
                        )),
                    _ => unexpected_token!(this; name)
                }
            }),

            DeclarationSubstate::AfterEncoding => match t {
                Token::Whitespace(_) => None,
                Token::EqualsSign => self.into_state_continue(State::InsideDeclaration(DeclarationSubstate::InsideEncodingValue)),
                _ => unexpected_token!(t)
            },

            DeclarationSubstate::InsideEncodingValue => self.read_attribute_value(t, |this, value| {
                this.data.encoding = Some(value);
                this.into_state_continue(State::InsideDeclaration(DeclarationSubstate::BeforeStandaloneDecl))
            }),

            DeclarationSubstate::BeforeStandaloneDecl => match t {
                Token::Whitespace(_) => None,  // skip whitespace
                Token::Character('s') => self.into_state_continue(State::InsideDeclaration(DeclarationSubstate::InsideStandaloneDecl)),
                Token::ProcessingInstructionEnd => emit_start_document(self),
                _ => unexpected_token!(t)
            },

            DeclarationSubstate::InsideStandaloneDecl => self.read_qualified_name(t, QualifiedNameTarget::AttributeNameTarget, |this, token, name| {
                match name.local_name.as_slice() {
                    "tandalone" if name.namespace.is_none() =>
                        this.into_state_continue(State::InsideDeclaration(
                            if token == Token::EqualsSign {
                                DeclarationSubstate::InsideStandaloneDeclValue
                            } else {
                                DeclarationSubstate::AfterStandaloneDecl
                            }
                        )),
                    _ => unexpected_token!(this; name)
                }
            }),

            DeclarationSubstate::AfterStandaloneDecl => match t {
                Token::Whitespace(_) => None,
                Token::EqualsSign => self.into_state_continue(State::InsideDeclaration(DeclarationSubstate::InsideStandaloneDeclValue)),
                _ => unexpected_token!(t)
            },

            DeclarationSubstate::InsideStandaloneDeclValue => self.read_attribute_value(t, |this, value| {
                let standalone = match value.as_slice() {
                    "yes" => Some(true),
                    "no"  => Some(false),
                    _     => None
                };
                if standalone.is_some() {
                    this.data.standalone = standalone;
                    this.into_state_continue(State::InsideDeclaration(DeclarationSubstate::AfterStandaloneDeclValue))
                } else {
                    Some(self_error!(this; "Invalid standalone declaration value: {}", value.to_string()))
                }
            }),

            DeclarationSubstate::AfterStandaloneDeclValue => match t {
                Token::Whitespace(_) => None,  // skip whitespace
                Token::ProcessingInstructionEnd => emit_start_document(self),
                _ => unexpected_token!(t)
            }
        }
    }

    #[inline]
    fn emit_start_element(&mut self, emit_end_element: bool) -> Option<XmlEvent> {
        let mut name = self.data.take_element_name().unwrap();
        let mut attributes = self.data.take_attributes();

        // check whether the name prefix is bound and fix its namespace
        match self.nst.get(&name.prefix) {
            Some("") => name.namespace = None,  // default namespace
            Some(ns) => name.namespace = Some(ns.to_string()),
            None => return Some(self_error!(self; "Element {} prefix is unbound", name.to_string()))
        }

        // check and fix accumulated attributes prefixes
        for attr in attributes.iter_mut() {
            match self.nst.get(&attr.name.prefix) {
                Some("") => attr.name.namespace = None,  // default namespace
                Some(ns) => attr.name.namespace = Some(ns.to_string()),
                None => return Some(self_error!(self; "Attribute {} prefix is unbound", attr.name.to_string()))
            }
        }

        if emit_end_element {
            self.pop_namespace = true;
            self.next_event = Some(XmlEvent::EndElement {
                name: name.clone()
            });
        } else {
            self.est.push(name.clone());
        }
        let namespace = self.nst.squash();
        self.into_state_emit(State::OutsideTag, XmlEvent::StartElement {
            name: name,
            attributes: attributes.into_iter().map(|a| a.into_attribute()).collect(),
            namespace: namespace
        })
    }

    fn inside_opening_tag(&mut self, t: Token, s: OpeningTagSubstate) -> Option<XmlEvent> {
        macro_rules! unexpected_token(($t:expr) => (Some(self_error!(self; "Unexpected token inside opening tag: {}", $t.to_string()))));
        match s {
            OpeningTagSubstate::InsideName => self.read_qualified_name(t, QualifiedNameTarget::OpeningTagNameTarget, |this, token, name| {
                match name.prefix_as_ref() {
                    Some(prefix) if prefix == namespace::NS_XML_PREFIX ||
                                    prefix == namespace::NS_XMLNS_PREFIX =>
                        Some(self_error!(this; "'{:?}' cannot be an element name prefix", name.prefix)),
                    _ => {
                        this.data.element_name = Some(name.clone());
                        match token {
                            Token::TagEnd => this.emit_start_element(false),
                            Token::EmptyTagEnd => this.emit_start_element(true),
                            Token::Whitespace(_) => this.into_state_continue(State::InsideOpeningTag(OpeningTagSubstate::InsideTag)),
                            _ => unreachable!()
                        }
                    }
                }
            }),

            OpeningTagSubstate::InsideTag => match t {
                Token::Whitespace(_) => None,  // skip whitespace
                Token::Character(c) if is_name_start_char(c) => {
                    self.buf.push(c);
                    self.into_state_continue(State::InsideOpeningTag(OpeningTagSubstate::InsideAttributeName))
                }
                Token::TagEnd => self.emit_start_element(false),
                Token::EmptyTagEnd => self.emit_start_element(true),
                _ => unexpected_token!(t.to_string())
            },

            OpeningTagSubstate::InsideAttributeName => self.read_qualified_name(t, QualifiedNameTarget::AttributeNameTarget, |this, token, name| {
                this.data.attr_name = Some(name);
                match token {
                    Token::Whitespace(_) => this.into_state_continue(State::InsideOpeningTag(OpeningTagSubstate::AfterAttributeName)),
                    Token::EqualsSign => this.into_state_continue(State::InsideOpeningTag(OpeningTagSubstate::InsideAttributeValue)),
                    _ => unreachable!()
                }
            }),

            OpeningTagSubstate::AfterAttributeName => match t {
                Token::Whitespace(_) => None,
                Token::EqualsSign => self.into_state_continue(State::InsideOpeningTag(OpeningTagSubstate::InsideAttributeValue)),
                _ => unexpected_token!(t.to_string())
            },

            OpeningTagSubstate::InsideAttributeValue => self.read_attribute_value(t, |this, value| {
                let name = this.data.take_attr_name().unwrap();  // unwrap() will always succeed here
                match name.prefix_as_ref() {
                    // declaring a new prefix; it is sufficient to check prefix only
                    // because "xmlns" prefix is reserved
                    Some(prefix) if prefix == namespace::NS_XMLNS_PREFIX => {
                        let ln = name.local_name.as_slice();
                        if ln == namespace::NS_XMLNS_PREFIX {
                            Some(self_error!(this; "Cannot redefine '{}' prefix", namespace::NS_XMLNS_PREFIX))
                        } else if ln == namespace::NS_XML_PREFIX && value.as_slice() != namespace::NS_XML_URI {
                            Some(self_error!(this; "'{}' prefix cannot be rebound to another value", namespace::NS_XML_PREFIX))
                        } else if value.is_empty() {
                            Some(self_error!(this; "Cannot undefine a prefix: {}", ln))
                        } else {
                            this.nst.put(Some(name.local_name.clone()), value);
                            this.into_state_continue(State::InsideOpeningTag(OpeningTagSubstate::InsideTag))
                        }
                    }

                    // declaring default namespace
                    None if name.local_name.as_slice() == namespace::NS_XMLNS_PREFIX =>
                        match value.as_slice() {
                            val if val == namespace::NS_XMLNS_PREFIX ||
                                   val == namespace::NS_XML_PREFIX =>
                                Some(self_error!(this; "Namespace '{}' cannot be default", value)),
                            _ => {
                                this.nst.put(None, value.clone());
                                this.into_state_continue(State::InsideOpeningTag(OpeningTagSubstate::InsideTag))
                            }
                        },

                    // Plain attribute
                    _ => {
                        this.data.attributes.push(AttributeData {
                            name: name.clone(),
                            value: value
                        });
                        this.into_state_continue(State::InsideOpeningTag(OpeningTagSubstate::InsideTag))
                    }
                }
            })
        }
    }

    #[inline]
    fn emit_end_element(&mut self) -> Option<XmlEvent> {
        let mut name = self.data.take_element_name().unwrap();

        // check whether the name prefix is bound and fix its namespace
        match self.nst.get(&name.prefix) {
            Some("") => name.namespace = None,  // default namespace
            Some(ns) => name.namespace = Some(ns.to_string()),
            None => return Some(self_error!(self; "Element {} prefix is unbound", name.to_string()))
        }

        let op_name = self.est.pop().unwrap();

        if name == op_name {
            self.pop_namespace = true;
            self.into_state_emit(State::OutsideTag, XmlEvent::EndElement { name: name })
        } else {
            Some(self_error!(self; "Unexpected closing tag: {}, expected {}", name.to_string(), op_name.to_string()))
        }
    }

    fn inside_closing_tag_name(&mut self, t: Token, s: ClosingTagSubstate) -> Option<XmlEvent> {
        match s {
            ClosingTagSubstate::CTInsideName => self.read_qualified_name(t, QualifiedNameTarget::ClosingTagNameTarget, |this, token, name| {
                match name.prefix_as_ref() {
                    Some(prefix) if prefix == namespace::NS_XML_PREFIX ||
                                    prefix == namespace::NS_XMLNS_PREFIX =>
                        Some(self_error!(this; "'{:?}' cannot be an element name prefix", name.prefix)),
                    _ => {
                        this.data.element_name = Some(name.clone());
                        match token {
                            Token::Whitespace(_) => this.into_state_continue(State::InsideClosingTag(ClosingTagSubstate::CTAfterName)),
                            Token::TagEnd => this.emit_end_element(),
                            _ => Some(self_error!(this; "Unexpected token inside closing tag: {}", token.to_string()))
                        }
                    }
                }
            }),
            ClosingTagSubstate::CTAfterName => match t {
                Token::Whitespace(_) => None,  //  Skip whitespace
                Token::TagEnd => self.emit_end_element(),
                _ => Some(self_error!(self; "Unexpected token inside closing tag: {}", t.to_string()))
            }
        }
    }

    fn inside_comment(&mut self, t: Token) -> Option<XmlEvent> {
        match t {
            // Double dash is illegal inside a comment
            Token::Chunk(ref s) if s.as_slice() == "--" => Some(self_error!(self; "Unexpected token inside a comment: --")),

            Token::CommentEnd if self.config.ignore_comments => {
                self.lexer.enable_errors();
                self.into_state_continue(State::OutsideTag)
            }

            Token::CommentEnd => {
                self.lexer.enable_errors();
                let data = self.take_buf();
                self.into_state_emit(State::OutsideTag, XmlEvent::Comment(data))
            }

            _ if self.config.ignore_comments => None,  // Do not modify buffer if ignoring the comment

            _ => self.append_str_continue(t.to_string().as_slice()),
        }
    }

    fn inside_cdata(&mut self, t: Token) -> Option<XmlEvent> {
        match t {
            Token::CDataEnd => {
                self.lexer.enable_errors();
                let event = if self.config.cdata_to_characters {
                    None
                } else {
                    let data = self.take_buf();
                    Some(XmlEvent::CData(data))
                };
                self.into_state(State::OutsideTag, event)
            }

            Token::Whitespace(_) => self.append_str_continue(t.to_string().as_slice()),

            _ => {
                self.inside_whitespace = false;
                self.append_str_continue(t.to_string().as_slice())
            }
        }
    }

    fn inside_reference(&mut self, t: Token, prev_st: State) -> Option<XmlEvent> {
        use std::char;
        use std::num::from_str_radix;

        match t {
            Token::Character(c) if !self.data.ref_data.is_empty() && is_name_char(c) ||
                             self.data.ref_data.is_empty() && (is_name_start_char(c) || c == '#') => {
                self.data.ref_data.push(c);
                None
            }

            Token::ReferenceEnd => {
                // TODO: check for unicode correctness
                let name = self.data.take_ref_data();
                let name_len = name.len();  // compute once
                let c = match name.as_slice() {
                    "lt"   => Ok('<'),
                    "gt"   => Ok('>'),
                    "amp"  => Ok('&'),
                    "apos" => Ok('\''),
                    "quot" => Ok('"'),
                    ""     => Err(self_error!(self; "Encountered empty entity")),
                    _ if name_len > 2 && &name[0..2] == "#x" => {
                        let num_str = &name[2..name_len];
                        if num_str == "0" {
                            Err(self_error!(self; "Null character entity is not allowed"))
                        } else {
                            match from_str_radix(num_str, 16).and_then(char::from_u32) {
                                Some(c) => Ok(c),
                                None    => Err(self_error!(self; "Invalid hexadecimal character number in an entity: {}", name))
                            }
                        }
                    }
                    _ if name_len > 1 && name.as_slice().char_at(0) == '#' => {
                        let num_str = &name[1..name_len];
                        if num_str == "0" {
                            Err(self_error!(self; "Null character entity is not allowed"))
                        } else {
                            match from_str_radix(num_str, 10).and_then(char::from_u32) {
                                Some(c) => Ok(c),
                                None    => Err(self_error!(self; "Invalid decimal character number in an entity: {}", name))
                            }
                        }
                    },
                    _ => Err(self_error!(self; "Unexpected entity: {}", name))
                };
                match c {
                    Ok(c) => {
                        self.buf.push(c);
                        self.into_state_continue(prev_st)
                    }
                    Err(e) => Some(e)
                }
            }

            _ => Some(self_error!(self; "Unexpected token inside an entity: {}", t.to_string()))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::BufReader;

    use name::OwnedName;
    use attribute::OwnedAttribute;
    use reader::parser::PullParser;
    use reader::ParserConfig;
    use reader::events::XmlEvent;

    fn new_parser() -> PullParser {
        PullParser::new(ParserConfig::new())
    }

    macro_rules! expect_event(
        ($r:expr, $p:expr, $t:pat) => (
            match $p.next(&mut $r) {
                $t => {}
                e => panic!("Unexpected event: {:?}", e)
            }
        );
        ($r:expr, $p:expr, $t:pat [ $c:expr ]) => (
            match $p.next(&mut $r) {
                $t if $c => {}
                e => panic!("Unexpected event: {:?}", e)
            }
        )
    );

    macro_rules! test_data(
        ($d:expr) => ({
            static DATA: &'static str = $d;
            let r = BufReader::new(DATA.as_bytes());
            let p = new_parser();
            (r, p)
        })
    );

    #[test]
    fn semicolon_in_attribute_value__issue_3() {
        let (mut r, mut p) = test_data!(r#"
            <a attr="zzz;zzz" />
        "#);

        expect_event!(r, p, XmlEvent::StartDocument { .. });
        expect_event!(r, p, XmlEvent::StartElement { ref name, ref attributes, ref namespace }
            [ *name == OwnedName::local("a") &&
               attributes.len() == 1 &&
               attributes[0] == OwnedAttribute::new(OwnedName::local("attr".to_string()), "zzz;zzz".to_string()) &&
               namespace.is_essentially_empty()
            ]
        );
        expect_event!(r, p, XmlEvent::EndElement { ref name } [ *name == OwnedName::local("a".to_string()) ]);
        expect_event!(r, p, XmlEvent::EndDocument);
    }

    #[test]
    fn opening_tag_in_attribute_value() {
        let (mut r, mut p) = test_data!(r#"
            <a attr="zzz<zzz" />
        "#);

        expect_event!(r, p, XmlEvent::StartDocument { .. });
        expect_event!(r, p, XmlEvent::Error(ref e)
            [ e.msg() == "Unexpected token inside attribute value: <" ]
        );
    }
}
