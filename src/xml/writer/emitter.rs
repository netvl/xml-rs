use std::io;
use std::iter;

use common;
use common::{OptionOps, Error, XmlVersion, Attribute, Name, NamespaceStack, Namespace, is_name_start_char, is_name_char, is_whitespace_char};
use events;
use events::XmlEvent;

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

pub trait Emitter {
    pub fn emit_start_document(&mut self, version: XmlVersion, encoding: &str, standalone: Option<bool>) -> EmitterResult<()>;
    pub fn emit_processing_instruction(&mut self, name: &str, data: Option<&str>) -> EmitterResult<()>;
    pub fn emit_empty_element(&mut self, name: Name, attributes: &[Attribute], namespace: &Namespace) -> EmitterResult<()>;
    pub fn emit_start_element(&mut self, name: Name, attributes: &[Attribute], namespace: &Namespace) -> EmitterResult<()>;
    pub fn emit_end_element(&mut self, name: Name) -> EmitterResult<()>;
    pub fn emit_cdata(&mut self, content: &str) -> EmitterResult<()>;
    pub fn emit_characters(&mut self, content: &str) -> EmitterResult<()>;
}

pub struct Emitter {
    priv config: EmitterConfig,

    priv nst: NamespaceStack,

    priv indent_level: uint,
    priv indent_stack: Vec<u8>,

    priv start_document_emitted: bool
}

pub fn new(config: EmitterConfig) -> Emitter {
    Emitter {
        config: config,

        nst: NamespaceStack::empty(),

        indent_level: 0,
        indent_stack: vec!(),

        start_document_emitted: false
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
    ($before_name:ident ($arg:expr) and $after_name:ident, $body:expr) => ({
        try!(self.$before_name($arg));
        let result = $body;
        self.$after_name();
        result
    })
)

macro_rules! if_present(
    ($opt:ident, $body:expr) => ($opt.map(|$opt| $body).unwrap_or(Ok(())))
)

static WROTE_NOTHING: u8 = 0;
static WROTE_MARKUP: u8 = 1;
static WROTE_TEXT: u8 = 2;

impl Emitter {
    /// Returns current state of namespaces.
    #[inline]
    pub fn namespace_stack<'a>(&'a self) -> &'a NamespaceStack {
        & self.nst
    }

    #[inline]
    fn wrote_text(&self) -> bool {
        *self.indent_stack.last().unwrap() & WROTE_TEXT > 0
    }

    #[inline]
    fn wrote_markup(&self) -> bool {
        *self.indent_stack.last().unwrap() & WROTE_MARKUP > 0
    }

    #[inline]
    fn set_wrote_text(&mut self) {
        *self.indent_stack.mut_last().unwrap() = WROTE_TEXT;
    }

    #[inline]
    fn set_wrote_markup(&mut self) {
        *self.indent_stack.mut_last().unwrap() = WROTE_MARKUP;
    }

    #[inline]
    fn reset_state(&mut self) {
        *self.indent_stack.mut_last().unwrap() = WROTE_NOTHING;
    }

    fn write_newline(&mut self, target: &mut Writer, level: uint) -> EmitterResult<()> {
        try!(target.write_str(self.config.line_separator));
        for i in iter::range(0, level) {
            try!(target.write_str(self.config.indent_string));
        }
    }

    fn before_markup(&mut self, target: &mut Writer) -> EmitterResult<()> {
        if !self.wrote_text() && (self.indent_level > 0 || self.wrote_markup()) {
            try!(self.write_newline(target, self.indent_level));
            if self.indent_level > 0 && self.config.indent_string.len() > 0 {
                self.after_markup();
            }
        }
    }

    fn after_markup(&mut self) {
        self.set_wrote_markup();
    }

    fn before_start_element(&mut self, target: &mut Writer) -> EmitterResult<()> {
        try!(self.before_markup(target));
        self.indent_stack.push(0);
    }

    fn after_start_element(&mut self) {
        self.after_markup();
        self.indent_level += 1;
    }

    fn before_end_element(&mut self, target: &mut Writer) -> EmitterResult<()> {
        if self.indent_level > 0 && self.wrote_markup() && !self.wrote_text() {
            self.write_newline(target, self.indent_level - 1)
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

    pub fn emit_start_document(&mut self, target: &mut Writer, version: XmlVersion, encoding: &str, standalone: Option<bool>) -> EmitterResult<()> {
        if self.start_document_emitted {
            return Err(error(DocumentStartAlreadyEmitted, "Document start is already emitted"));
        }
        self.start_document_emitted = true;

        wrapped_with!(before_markup(target) and after_markup,
            io_chain!(
                write!(target, "<?xml version=\"{}\" encoding=\"{}\"", version, encoding),

                if_present!(standalone, write!(target, " standalone=\"{}\"", if standalone { "yes" } else { "no" })),

                write!(target, "?>")
            )
        )
    }

    fn check_document_started(&mut self, target: &mut Writer) -> EmitterResult<()> {
        if !self.start_document_emitted && self.config.write_document_declaration {
            self.emit_start_document(target, common::Version10, "utf-8", None)
        } else {
            Ok(())
        }
    }

    pub fn emit_processing_instruction(&mut self, target: &mut Writer, name: &str, data: Option<&str>) -> EmitterResult<()> {
        try!(self.check_document_started(target));

        wrapped_with!(before_markup(target) and after_markup,
            io_chain!(
                write!(target, "<?{}", name),

                if_present!(data, write!(target, " {}", data)),

                write!(target, "?>")
            )
        )
    }

    fn emit_start_element_initial(&mut self, target: &mut Writer, name: Name, attributes: &[Attribute], namespace: &Namespace) -> EmitterResult<()> {
        try!(self.check_document_started(target));

        try!(self.before_start_element(target));

        io_chain!(
            write!(target, "<{}", name.to_str_proper()),

            self.emit_namespace_attributes(namespace),

            self.emit_attributes(attributes)
        )
    }

    pub fn emit_empty_element(&mut self, target: &mut Writer, name: Name, attributes: &[Attribute], namespace: &Namespace) -> EmitterResult<()> {
        try!(self.emit_start_element_initial(target, name, attributes, namespace));

        io_wrap(write!("/>"));
    }

    pub fn emit_start_element(&mut self, target: &mut Writer, name: Name, attributes: &[Attribute], namespace: &Namespace) -> EmitterResult<()> {
        try!(self.emit_start_element_initial(target, name, attributes, namespace));

        try!(self.check_document_started(target));

        wrapped_with!(before_start_element(target) and after_start_element,
            io_chain!(
                write!(target, "<{}", name.to_str_proper(),

                self.emit_namespace_attributes(namespace),

                self.emit_attributes(attributes),

                write(
            )
        )
    }

    pub fn emit_end_element(&mut self, target: &mut Writer, name: Name) -> EmitterResult<()> {
        Ok(())
    }

    pub fn emit_cdata(&mut self, target: &mut Writer, content: &str) -> EmitterResult<()> {
        Ok(())
    }

    pub fn emit_characters(&mut self, target: &mut Writer, content: &str) -> EmitterResult<()> {
        Ok(())
    }

    pub fn emit_whitespace(&mut self, target: &mut Writer, content: &str) -> EmitterResult<()> {
        Ok(())
    }
}
