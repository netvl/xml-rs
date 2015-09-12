extern crate xml;

use std::io::{BufReader, SeekFrom};
use std::io::prelude::*;
use std::fs::File;

use xml::reader::EventReader;
use xml::writer::{EventWriter, EmitterConfig};

#[test]
fn reading_writing_equal_with_namespaces() {
    let mut f = File::open("tests/documents/sample_2.xml").unwrap();
    let mut b = Vec::new();

    {
        let r = EventReader::new(BufReader::new(&mut f));
        let mut w = EventWriter::new_with_config(&mut b, EmitterConfig::default().perform_indent(true));

        for e in r {
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
    let mut f2 = File::create("/tmp/test.xml").unwrap();
    f2.write_all(bs.as_bytes()).unwrap();

    assert_eq!(fs.trim(), bs.trim());
}

#[test]
fn writing_simple() {
    use xml::writer::events::XmlEvent;

    let mut b = Vec::new();

    {
        let mut w = EmitterConfig::new().write_document_declaration(false).create_writer(&mut b);

        w.write(XmlEvent::start_element("h:hello").ns("h", "urn:hello-world")).unwrap();
        w.write("hello world").unwrap();
        w.write(XmlEvent::end_element()).unwrap();
    }

    assert_eq!(&*b, br#"<h:hello xmlns:h="urn:hello-world">hello world</h:hello>"# as &[u8]);
}
