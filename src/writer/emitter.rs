use std::io;
use std::iter;
use std::fmt;

use common::{XmlVersion, Attribute, Name, escape_str};
use namespace::{NamespaceStack, NamespaceIterable};

use writer::config::EmitterConfig;

pub enum EmitterErrorKind {
    IoError,
    DocumentStartAlreadyEmitted,
    UnexpectedEvent,
    InvalidWhitespaceEvent
}

pub struct EmitterError {
    kind: EmitterErrorKind,
    message: &'static str,
    cause: Option<io::IoError>
}

impl fmt::Show for EmitterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "Emitter error: {}", self.message));
        if self.cause.is_some() {
            write!(f, "; caused by: {}", *self.cause.as_ref().unwrap())
        } else {
            Ok(())
        }
    }
}

pub fn error(kind: EmitterErrorKind, message: &'static str) -> EmitterError {
    EmitterError {
        kind: kind,
        message: message,
        cause: None
    }
}

#[inline]
fn io_error(err: io::IoError) -> EmitterError {
    EmitterError { kind: IoError, message: "Input/output error", cause: Some(err) }
}

pub type EmitterResult<T> = Result<T, EmitterError>;

#[inline]
pub fn io_wrap<T>(result: io::IoResult<T>) -> EmitterResult<T> {
    result.map_err(io_error)
}

pub struct Emitter {
    config: EmitterConfig,

    nst: NamespaceStack,

    indent_level: uint,
    indent_stack: Vec<IndentFlags>,

    start_document_emitted: bool
}

impl Emitter {
    pub fn new(config: EmitterConfig) -> Emitter {
        Emitter {
            config: config,

            nst: NamespaceStack::empty(),

            indent_level: 0,
            indent_stack: vec!(IndentFlags::empty()),

            start_document_emitted: false
        }
    }
}

macro_rules! io_try(
    ($e:expr) => (
        match $e {
            Ok(value) => value,
            Err(err) => return Err(io_error(err))
        }
    )
)

macro_rules! io_chain(
    ($e:expr) => (io_wrap($e));
    ($e:expr, $($rest:expr),+) => ({
        io_try!($e);
        io_chain!($($rest),+)
    })
)

macro_rules! wrapped_with(
    ($_self:ident; $before_name:ident ($arg:expr) and $after_name:ident, $body:expr) => ({
        try!($_self.$before_name($arg));
        let result = $body;
        $_self.$after_name();
        result
    })
)

macro_rules! if_present(
    ($opt:ident, $body:expr) => ($opt.map(|$opt| $body).unwrap_or(Ok(())))
)

bitflags!(
    flags IndentFlags: u8 {
        const WROTE_NOTHING = 0,
        const WROTE_MARKUP  = 1,
        const WROTE_TEXT    = 2
    }
)

impl Emitter {
    /// Returns current state of namespaces.
    #[inline]
    pub fn namespace_stack<'a>(&'a self) -> &'a NamespaceStack {
        & self.nst
    }

    #[inline]
    fn wrote_text(&self) -> bool {
        self.indent_stack.last().unwrap().contains(WROTE_TEXT)
    }

    #[inline]
    fn wrote_markup(&self) -> bool {
        self.indent_stack.last().unwrap().contains(WROTE_MARKUP)
    }

    #[inline]
    fn set_wrote_text(&mut self) {
        *self.indent_stack.last_mut().unwrap() = WROTE_TEXT;
    }

    #[inline]
    fn set_wrote_markup(&mut self) {
        *self.indent_stack.last_mut().unwrap() = WROTE_MARKUP;
    }

    #[inline]
    fn reset_state(&mut self) {
        *self.indent_stack.last_mut().unwrap() = WROTE_NOTHING;
    }

    fn write_newline<W: Writer>(&mut self, target: &mut W, level: uint) -> EmitterResult<()> {
        io_try!(target.write_str(self.config.line_separator.as_slice()));
        for _ in iter::range(0, level) {
            io_try!(target.write_str(self.config.indent_string.as_slice()));
        }
        Ok(())
    }

    fn before_markup<W: Writer>(&mut self, target: &mut W) -> EmitterResult<()> {
        if !self.wrote_text() && (self.indent_level > 0 || self.wrote_markup()) {
            let indent_level = self.indent_level;
            try!(self.write_newline(target, indent_level));
            if self.indent_level > 0 && self.config.indent_string.len() > 0 {
                self.after_markup();
            }
        }
        Ok(())
    }

    fn after_markup(&mut self) {
        self.set_wrote_markup();
    }

    fn before_start_element<W: Writer>(&mut self, target: &mut W) -> EmitterResult<()> {
        try!(self.before_markup(target));
        self.indent_stack.push(WROTE_NOTHING);
        Ok(())
    }

    fn after_start_element(&mut self) {
        self.after_markup();
        self.indent_level += 1;
    }

    fn before_end_element<W: Writer>(&mut self, target: &mut W) -> EmitterResult<()> {
        if self.indent_level > 0 && self.wrote_markup() && !self.wrote_text() {
            let indent_level = self.indent_level;
            self.write_newline(target, indent_level - 1)
        } else {
            Ok(())
        }
    }

    fn after_end_element(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
            self.indent_stack.pop();
        }
        self.set_wrote_markup();
    }

    fn after_text(&mut self) {
        self.set_wrote_text();
    }

    pub fn emit_start_document<W: Writer>(&mut self, target: &mut W, version: XmlVersion, encoding: &str, standalone: Option<bool>) -> EmitterResult<()> {
        if self.start_document_emitted {
            return Err(error(DocumentStartAlreadyEmitted, "Document start is already emitted"));
        }
        self.start_document_emitted = true;

        wrapped_with!(self; before_markup(target) and after_markup,
            io_chain!(
                write!(target, "<?xml version=\"{}\" encoding=\"{}\"", version, encoding),

                if_present!(standalone, write!(target, " standalone=\"{}\"", if standalone { "yes" } else { "no" })),

                write!(target, "?>")
            )
        )
    }

    fn check_document_started<W: Writer>(&mut self, target: &mut W) -> EmitterResult<()> {
        if !self.start_document_emitted && self.config.write_document_declaration {
            self.emit_start_document(target, common::Version10, "utf-8", None)
        } else {
            Ok(())
        }
    }

    pub fn emit_processing_instruction<W: Writer>(&mut self, target: &mut W, name: &str, data: Option<&str>) -> EmitterResult<()> {
        try!(self.check_document_started(target));

        wrapped_with!(self; before_markup(target) and after_markup,
            io_chain!(
                write!(target, "<?{}", name),

                if_present!(data, write!(target, " {}", data)),

                write!(target, "?>")
            )
        )
    }

    fn emit_start_element_initial<'a, W: Writer,
                                  N: NamespaceIterable<'a, I>,
                                  I: Iterator<(Option<&'a str>, &'a str)>
                                 >(&mut self, target: &mut W, name: &Name, attributes: &[Attribute],
                                   namespace: &'a N) -> EmitterResult<()> {
        try!(self.check_document_started(target));

        try!(self.before_start_element(target));

        io_try!(write!(target, "<{}", name.to_str_proper()));

        try!(self.emit_namespace_attributes(target, namespace));

        self.emit_attributes(target, attributes)
    }

    pub fn emit_empty_element<'a, W: Writer,
                              N: NamespaceIterable<'a, I>,
                              I: Iterator<(Option<&'a str>, &'a str)>
                             >(&mut self, target: &mut W, name: &Name, attributes: &[Attribute],
                               namespace: &'a N) -> EmitterResult<()> {
        try!(self.emit_start_element_initial(target, name, attributes, namespace));

        io_wrap(write!(target, "/>"))
    }

    pub fn emit_start_element<'a, W: Writer,
                              N: NamespaceIterable<'a, I>,
                              I: Iterator<(Option<&'a str>, &'a str)>
                             >(&mut self, target: &mut W, name: &Name, attributes: &[Attribute],
                               namespace: &'a N) -> EmitterResult<()> {
        try!(self.emit_start_element_initial(target, name, attributes, namespace));

        io_wrap(write!(target, ">"))
    }

    pub fn emit_namespace_attributes<'a, W: Writer,
                                     N: NamespaceIterable<'a, I>,
                                     I: Iterator<(Option<&'a str>, &'a str)>
                                    >(&mut self, target: &mut W, namespace: &'a N) -> EmitterResult<()> {
        for (prefix, uri) in namespace.uri_mappings() {
            io_try!(match prefix {
                Some("xmlns") | Some("xml") => Ok(()),  // emit nothing
                Some(prefix) => write!(target, " xmlns:{}=\"{}\"", prefix, uri),
                None => if !uri.is_empty() {  // emit xmlns only if it is overridden
                    write!(target, " xmlns=\"{}\"", uri)
                } else { Ok(()) }
            });
        }
        Ok(())
    }

    pub fn emit_attributes<W: Writer>(&mut self, target: &mut W, attributes: &[Attribute]) -> EmitterResult<()> {
        for attr in attributes.iter() {
            io_try!(write!(target, " {}=\"{}\"", attr.name.to_str_proper(), escape_str(attr.value.as_slice())))
        }
        Ok(())
    }

    pub fn emit_end_element<W: Writer>(&mut self, target: &mut W, name: &Name) -> EmitterResult<()> {
        wrapped_with!(self; before_end_element(target) and after_end_element,
            io_wrap(write!(target, "</{}>", name.to_str_proper()))
        )
    }

    pub fn emit_cdata<W: Writer>(&mut self, target: &mut W, content: &str) -> EmitterResult<()> {
        if self.config.cdata_to_characters {
            self.emit_characters(target, content)
        } else {
            io_try!(target.write_str("<![CDATA["));
            io_try!(target.write_str(content));
            io_try!(target.write_str("]]>"));
            self.after_text();
            Ok(())
        }
    }

    pub fn emit_characters<W: Writer>(&mut self, target: &mut W, content: &str) -> EmitterResult<()> {
        io_try!(target.write_str(escape_str(content).as_slice()));
        self.after_text();
        Ok(())
    }

    pub fn emit_comment<W: Writer>(&mut self, target: &mut W, content: &str) -> EmitterResult<()> {
        Ok(())  // TODO: proper write
    }
}
