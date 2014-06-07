//!
//!

use std::io;

use common;
use common::{Error, XmlVersion, Attribute, Name, is_name_start_char, is_name_char, is_whitespace_char};
use namespace;
use namespace::{Namespace, NamespaceStack};
use writer::emitter;
use writer::emitter::{Emitter, EmitterResult, error, io_wrap, UnexpectedEvent};
use writer::events;
use writer::events::XmlEvent;

use writer::config::EmitterConfig;

pub struct EventEmitter {
    emitter: Emitter
}

pub fn new(config: EmitterConfig) -> EventEmitter {
    EventEmitter {
        emitter: emitter::new(config)
    }
}

type Chunk = EmitterResult<String>;

impl EventEmitter {
    pub fn emit<W: Writer>(&mut self, mut target: W, event: XmlEvent) -> EmitterResult<()> {
        let chunk = try!(self.dispatch_event(event));
        io_wrap(target.write_str(chunk.as_slice()))
    }

    fn dispatch_event(&mut self, event: XmlEvent) -> Chunk {
        match event {
            events::StartDocument { version, encoding, standalone } => self.emit_start_document(version, encoding, standalone),
            events::EndDocument => self.emit_end_document(),
            events::ProcessingInstruction { name, data } => self.emit_processing_instruction(name, data),
            events::StartElement { name, attributes, namespace } => self.emit_start_element(name, attributes, namespace),
            events::EndElement { name } => self.emit_end_element(name),
            events::Comment(content) => self.emit_comment(content),
            events::CData(content) => self.emit_cdata(content),
            events::Characters(content) => self.emit_characters(content),
            events::Whitespace(content) => self.emit_whitespace(content),
            events::Error(_) => Err(error(UnexpectedEvent, "Error event cannot be emitted"))
        }
    }

    fn emit_start_document(&mut self, version: XmlVersion, encoding: String, standalone: Option<bool>) -> Chunk {
        Ok(String::new())
    }

    fn emit_end_document(&mut self) -> Chunk {
        Ok(String::new())
    }

    fn emit_processing_instruction(&mut self, name: String, data: Option<String>) -> Chunk {
        Ok(String::new())
    }

    fn emit_start_element(&mut self, name: Name, attributes: Vec<Attribute>, namespace: Namespace) -> Chunk {
        Ok(String::new())
    }

    fn emit_end_element(&mut self, name: Name) -> Chunk {
        Ok(String::new())
    }

    fn emit_comment(&mut self, content: String) -> Chunk {
        Ok(String::new())
    }

    fn emit_cdata(&mut self, content: String) -> Chunk {
        Ok(String::new())
    }

    fn emit_characters(&mut self, content: String) -> Chunk {
        Ok(String::new())
    }

    fn emit_whitespace(&mut self, content: String) -> Chunk {
        Ok(String::new())
    }
}
