extern crate xml;

use std::io::{BufReader, SeekFrom};
use std::io::prelude::*;
use std::fs::File;
use std::str;

use xml::reader::EventReader;
use xml::writer::EmitterConfig;

macro_rules! unwrap_all {
    ($($e:expr);+) => {{
        $($e.unwrap();)+
    }}
}

#[test]
fn reading_writing_equal_with_namespaces() {
    let mut f = File::open("tests/documents/sample_2.xml").unwrap();
    let mut b = Vec::new();

    {
        let r = EventReader::new(BufReader::new(&mut f));
        let mut w = EmitterConfig::default().perform_indent(true).create_writer(&mut b);

        for e in r {
            match e {
                Ok(e) => match e.as_writer_event() {
                    Some(e) => match w.write(e) {
                        Ok(_) => {},
                        Err(e) => panic!("Writer error: {:?}", e)
                    },
                    None => println!("Non-writer event: {:?}", e)
                },
                Err(e) => panic!("Error: {}", e)
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

    assert_eq!(
        str::from_utf8(&b).unwrap(),
        r#"<h:hello xmlns:h="urn:hello-world">hello world</h:hello>"#
    );
}

#[test]
fn writing_empty_elements_with_normalizing() {
    use xml::writer::events::XmlEvent;

    let mut b = Vec::new();

    {
        let mut w = EmitterConfig::new().write_document_declaration(false).create_writer(&mut b);

        unwrap_all! {
            w.write(XmlEvent::start_element("hello"));
            w.write(XmlEvent::start_element("world"));
            w.write(XmlEvent::end_element());
            w.write(XmlEvent::end_element())
        }
    }

    assert_eq!(str::from_utf8(&b).unwrap(), r#"<hello><world /></hello>"#);
}

#[test]
fn writing_empty_elements_without_normalizing() {
    use xml::writer::events::XmlEvent;

    let mut b = Vec::new();

    {
        let mut w = EmitterConfig::new()
            .write_document_declaration(false)
            .normalize_empty_elements(false)
            .create_writer(&mut b);

        unwrap_all! {
            w.write(XmlEvent::start_element("hello"));
            w.write(XmlEvent::start_element("world"));
            w.write(XmlEvent::end_element());
            w.write(XmlEvent::end_element())
        }
    }

    assert_eq!(str::from_utf8(&b).unwrap(), r#"<hello><world></world></hello>"#);
}

#[test]
fn writing_comments_with_indentation() {
    use xml::writer::events::XmlEvent;

    let mut b = Vec::new();

    {
        let mut w = EmitterConfig::new()
            .write_document_declaration(false)
            .perform_indent(true)
            .create_writer(&mut b);

        unwrap_all! {
            w.write(XmlEvent::start_element("hello"));
            w.write(XmlEvent::start_element("world"));
            w.write(XmlEvent::comment("  this is a manually padded comment\t"));
            w.write(XmlEvent::comment("this is an unpadded comment"));
            w.write(XmlEvent::end_element());
            w.write(XmlEvent::end_element())
        }
    }

    assert_eq!(
        str::from_utf8(&b).unwrap(),
        "<hello>
  <world>
    <!--  this is a manually padded comment\t-->
    <!-- this is an unpadded comment -->
  </world>
</hello>");
}
