pub use self::emitter::EmitterResult as EventWriterResult;
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

    pub fn write(&mut self, event: XmlEvent) -> EventWriterResult<()> {
        match event {
            XmlEvent::StartDocument { version, encoding, standalone } =>
                self.emitter.emit_start_document(&mut self.sink, version, encoding.unwrap_or("UTF-8"), standalone),
            XmlEvent::ProcessingInstruction { name, data } =>
                self.emitter.emit_processing_instruction(&mut self.sink, name, data),
            XmlEvent::StartElement { name, attributes, namespace } =>
                self.emitter.emit_start_element(&mut self.sink, name, &attributes, namespace),
            XmlEvent::EndElement { name } =>
                self.emitter.emit_end_element(&mut self.sink, name),
            XmlEvent::Comment(content) =>
                self.emitter.emit_comment(&mut self.sink, content),
            XmlEvent::CData(content) =>
                self.emitter.emit_cdata(&mut self.sink, content),
            XmlEvent::Characters(content) =>
                self.emitter.emit_characters(&mut self.sink, content)
        }
    }
}

impl EventWriter<Vec<u8>> {
    #[inline]
    pub fn new_into_mem(sink: Vec<u8>) -> EventWriter<Vec<u8>> {
        EventWriter::new_into_mem_config(sink, EmitterConfig::new())
    }

    #[inline]
    pub fn new_into_mem_config(sink: Vec<u8>, config: EmitterConfig) -> EventWriter<Vec<u8>> {
        EventWriter::new_with_config(sink, config)
    }
}

#[cfg(test)]
mod tests {
    use std::io::{BufReader, SeekFrom};
    use std::io::prelude::*;
    use std::fs::File;
    use std::path::Path;

    use reader::EventReader;
    use writer::EventWriter;

    #[ignore]
    #[test]
    fn writer_test() {
        let mut f = File::open(Path::new("data/sample_1.xml")).unwrap();
        let mut b = Vec::new();

        {
            let mut r = EventReader::new(BufReader::new(&mut f));
            let mut w = EventWriter::new(&mut b);

            for e in r.events() {
                match e.as_writer_event() {
                    Some(e) => match w.write(e) {
                        Ok(_) => {},
                        Err(e) => panic!("Writer error: {:?}", e)
                    },
                    None => println!("Non-writer event: {:?}", e)
                }
            }
        }

        f.seek(SeekFrom::Start(0)).unwrap();
        let mut fs = String::new();
        f.read_to_string(&mut fs).unwrap();

        let bs = String::from_utf8(b).unwrap();

        assert_eq!(fs, bs)
    }
}
