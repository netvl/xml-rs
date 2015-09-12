pub use self::emitter::Result;
pub use self::config::EmitterConfig;

use self::emitter::Emitter;
use self::events::XmlEvent;

use std::io::prelude::*;

mod emitter;
pub mod config;
pub mod events;

pub struct EventWriter<W> {
    sink: W,
    emitter: Emitter
}

impl<W: Write> EventWriter<W> {
    #[inline]
    pub fn new(sink: W) -> EventWriter<W> {
        EventWriter::new_with_config(sink, EmitterConfig::new())
    }

    #[inline]
    pub fn new_with_config(sink: W, config: EmitterConfig) -> EventWriter<W> {
        EventWriter {
            sink: sink,
            emitter: Emitter::new(config)
        }
    }

    pub fn write<'a, E>(&mut self, event: E) -> Result<()> where E: Into<XmlEvent<'a>> {
        match event.into() {
            XmlEvent::StartDocument { version, encoding, standalone } =>
                self.emitter.emit_start_document(&mut self.sink, version, encoding.unwrap_or("UTF-8"), standalone),
            XmlEvent::ProcessingInstruction { name, data } =>
                self.emitter.emit_processing_instruction(&mut self.sink, name, data),
            XmlEvent::StartElement { name, attributes, namespace } => {
                self.emitter.namespace_stack_mut().push_empty().checked_target().extend(namespace.as_ref());
                self.emitter.emit_start_element(&mut self.sink, name, &attributes)
            }
            XmlEvent::EndElement { name } => {
                let r = self.emitter.emit_end_element(&mut self.sink, name);
                self.emitter.namespace_stack_mut().try_pop();
                r
            }
            XmlEvent::Comment(content) =>
                self.emitter.emit_comment(&mut self.sink, content),
            XmlEvent::CData(content) =>
                self.emitter.emit_cdata(&mut self.sink, content),
            XmlEvent::Characters(content) =>
                self.emitter.emit_characters(&mut self.sink, content)
        }
    }
}
