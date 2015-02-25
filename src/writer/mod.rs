pub use self::emitter::EmitterResult as EventWriterResult;
pub use self::config::EmitterConfig;

use std::old_io::MemWriter;

use self::emitter::Emitter;
use self::events::XmlEvent;

mod emitter;
pub mod config;
pub mod events;

pub struct EventWriter<'a, W: 'a> {
    sink: &'a mut W,
    emitter: Emitter
}

impl<'a, W: Writer> EventWriter<'a, W> {
    #[inline]
    pub fn new(sink: &'a mut W) -> EventWriter<'a, W> {
        EventWriter::new_with_config(sink, EmitterConfig::new())
    }

    #[inline]
    pub fn new_with_config(sink: &'a mut W, config: EmitterConfig) -> EventWriter<'a, W> {
        EventWriter {
            sink: sink,
            emitter: Emitter::new(config)
        }
    }

    pub fn write(&mut self, event: XmlEvent) -> EventWriterResult<()> {
        match event {
            XmlEvent::StartDocument { version, encoding, standalone } =>
                self.emitter.emit_start_document(self.sink, version, encoding.unwrap_or("UTF-8"), standalone),
            XmlEvent::ProcessingInstruction { name, data } =>
                self.emitter.emit_processing_instruction(self.sink, name, data),
            XmlEvent::StartElement { name, attributes, namespace } =>
                self.emitter.emit_start_element(self.sink, name, attributes.as_slice(), namespace),
            XmlEvent::EndElement { name } =>
                self.emitter.emit_end_element(self.sink, name),
            XmlEvent::Comment(content) =>
                self.emitter.emit_comment(self.sink, content),
            XmlEvent::CData(content) =>
                self.emitter.emit_cdata(self.sink, content),
            XmlEvent::Characters(content) =>
                self.emitter.emit_characters(self.sink, content)
        }
    }
}

impl<'a> EventWriter<'a, MemWriter> {
    #[inline]
    pub fn new_into_mem(sink: &'a mut MemWriter) -> EventWriter<'a, MemWriter> {
        EventWriter::new_into_mem_config(sink, EmitterConfig::new())
    }

    #[inline]
    pub fn new_into_mem_config(sink: &'a mut MemWriter, config: EmitterConfig) -> EventWriter<'a, MemWriter> {
        EventWriter::new_with_config(sink, config)
    }
}

#[cfg(test)]
mod tests {
    use std::old_io;
    use std::old_io::{File, BufferedReader, ByRefReader, ByRefWriter};

    use reader::EventReader;
    use writer::EventWriter;

    #[inline]
    fn reader_by_ref<R: Reader>(r: &mut R) -> old_io::RefReader<R> { r.by_ref() }

    #[ignore]
    #[test]
    fn writer_test() {
        let mut f = File::open(&Path::new("data/sample_1.xml")).unwrap();
        let mut b = Vec::new();

        {
            let mut r = EventReader::new(BufferedReader::new(reader_by_ref(&mut f)));
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

        f.seek(0, old_io::SeekSet);
        let fs = f.read_to_string().unwrap();

        let bs = String::from_utf8(b).unwrap();

        assert_eq!(fs, bs)
    }
}
