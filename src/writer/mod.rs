pub use EventWriterResult = self::emitter::EmitterResult;
pub use self::config::EmitterConfig;

use std::io::MemWriter;

use self::emitter::Emitter;
use self::events::XmlEvent;

mod emitter;
pub mod config;
pub mod events;

pub struct EventWriter<W> {
    sink: W,
    emitter: Emitter
}

impl<W: Writer> EventWriter<W> {
    #[inline]
    pub fn new(sink: W) -> EventWriter<W> {
        EventWriter::new_with_config(sink, EmitterConfig::new())
    }

    #[inline]
    pub fn new_with_config(sink: W, config: EmitterConfig) -> EventWriter<W> {
        EventWriter {
            sink: sink,
            emitter: emitter::new(config)
        }
    }

    pub fn write(&mut self, event: XmlEvent) -> EventWriterResult<()> {
        match event {
            events::StartDocument { version, encoding, standalone } => 
                self.emitter.emit_start_document(&mut self.sink, version, encoding.unwrap_or("UTF-8"), standalone),
            events::ProcessingInstruction { name, data } =>
                self.emitter.emit_processing_instruction(&mut self.sink, name, data),
            events::StartElement { name, attributes, namespace } =>
                self.emitter.emit_start_element(&mut self.sink, name, attributes, namespace),
            events::EndElement { name } => 
                self.emitter.emit_end_element(&mut self.sink, name),
            events::Comment(content) => 
                self.emitter.emit_comment(&mut self.sink, content),
            events::CData(content) => 
                self.emitter.emit_cdata(&mut self.sink, content),
            events::Characters(content) => 
                self.emitter.emit_characters(&mut self.sink, content)
        }
    }
}

impl EventWriter<MemWriter> {
    #[inline]
    pub fn new_into_mem(sink: MemWriter) -> EventWriter<MemWriter> {
        EventWriter::new_into_mem_config(sink, EmitterConfig::new())
    }

    #[inline]
    pub fn new_into_mem_config(sink: MemWriter, config: EmitterConfig) -> EventWriter<MemWriter> {
        EventWriter::new_with_config(sink, config)
    }
}
