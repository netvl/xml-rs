//!
//!

use std::io;

use common;
use common::{Error, XmlVersion, Name, NamespaceStack, is_name_start_char, is_name_char, is_whitespace_char};
use events;
use events::XmlEvent;

use writer::config::EmitterConfig;

pub enum EmitterErrorKind {
    IoError,
    UnexpectedEvent,
    InvalidWhitespaceEvent
}

pub struct EmitterError {
    kind: EmitterErrorKind,
    message: &'static str,
    cause: Option<io::IoError>
}

fn error(kind: EmitterErrorKind, message: &'static str) -> EmitterError {
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
pub type Chunk = EmitterResult<~str>;

#[inline]
fn io_wrap<T>(result: io::IoResult<T>) -> EmitterResult<T> {
    result.map_err(io_error)
}

pub struct EventEmitter {
    priv config: EmitterConfig,
    priv indent_level: uint,
    priv indent_stack: Vec<u8>,
    priv nst: NamespaceStack
}

pub fn new(config: EmitterConfig) -> Emitter {
    Emitter {
        config: config,
        nst: NamespaceStack::empty(),
        indent_level: 0,
        indent_stack: vec!()
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

impl EventEmitter {
    /// Returns current state of namespaces.
    pub fn namespace_stack<'a>(&'a self) -> &'a NamespaceStack {
        & self.nst
    }

    pub fn emit<W: Writer>(&mut self, mut target: W, event: XmlEvent) -> EmitterResult<()> {
        let chunk = try!(self.dispatch_event(event));
        io_wrap(target.write_str(chunk))
    }

    fn dispatch_event(&mut self, event: XmlEvent) -> Chunk {
        match event {
            StartDocument { version, encoding, standalone } =>
                self.emit_start_document(version, encoding, standalone),
            EndDocument => self.emit_end_document(),
            ProcessingInstruction { name, data } =>
                self.emit_processing_instruction(name, data),
            StartElement { name, attributes, namespace } =>
                self.emit_start_element(name, attributes, namespace),
            EndElement { name } =>
                self.emit_end_element(name),
            CData(content) => self.emit_cdata(content),
            Characters(content) => self.emit_characters(content),
            Whitespace(content) => self.emit_whitespace(content),
            Error(_) => error(UnexpectedError, "Error event cannot be emitted") 
        }
    }

    fn emit_start_document(version: XmlVersion, encoding: ~str, standalone: Option<bool>) -> Chunk {
        Ok(box "")
    }

    fn emit_processing_instruction(name: ~str, data: Option<~str>) -> Chunk {
        Ok(box "")
    }

    fn emit_start_element(name: Name, attributes: Vec<Attribute>, namespace: Namespace) -> Chunk {
        Ok(box "")
    }

    fn emit_end_element(name: Name) -> Chunk {
        Ok(box "")
    }

    fn emit_cdata(content: ~str) -> Chunk {
        Ok(box "")
    }

    fn emit_whitespace(content: ~str) -> Chunk {

    }
}
