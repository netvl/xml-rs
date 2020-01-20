extern crate xml;

use std::fs::File;
use std::io::prelude::*;
use std::io::{BufReader, SeekFrom};
use std::str;

use xml::{Event, EventBuilder, ReaderConfig, WriterConfig};

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
        let r = ReaderConfig::new().create_reader(BufReader::new(&mut f));
        let mut r = r.fused();
        let mut w = WriterConfig::new().perform_indent(true).create_writer(&mut b);

        while let Some(e) = r.next() {
            match e {
                Ok(e) => match w.write(e) {
                    Ok(_) => {}
                    Err(e) => panic!("Writer error: {:?}", e),
                },
                Err(e) => panic!("Error: {}", e),
            }
        }
    }

    f.seek(SeekFrom::Start(0)).unwrap();
    let mut fs = String::new();
    f.read_to_string(&mut fs).unwrap();

    let bs = String::from_utf8(b).unwrap();

    assert_eq!(fs.trim(), bs.trim());
}

#[test]
fn writing_simple() {
    let mut b = Vec::new();

    {
        let mut w = WriterConfig::new()
            .write_document_declaration(false)
            .create_writer(&mut b);

        w.write(EventBuilder::start_element("h:hello").ns("h", "urn:hello-world"))
            .unwrap();
        w.write(Event::text("hello world")).unwrap();
        w.write(EventBuilder::end_element()).unwrap();
    }

    assert_eq!(
        str::from_utf8(&b).unwrap(),
        r#"<h:hello xmlns:h="urn:hello-world">hello world</h:hello>"#
    );
}

#[test]
fn writing_empty_elements_with_normalizing() {
    let mut b = Vec::new();

    {
        let mut w = WriterConfig::new()
            .write_document_declaration(false)
            .create_writer(&mut b);

        unwrap_all! {
            w.write(EventBuilder::start_element("hello"));
            w.write(EventBuilder::start_element("world"));
            w.write(EventBuilder::end_element());
            w.write(EventBuilder::end_element())
        }
    }

    assert_eq!(str::from_utf8(&b).unwrap(), r#"<hello><world /></hello>"#);
}

#[test]
fn writing_empty_elements_without_normalizing() {
    let mut b = Vec::new();

    {
        let mut w = WriterConfig::new()
            .write_document_declaration(false)
            .normalize_empty_elements(false)
            .create_writer(&mut b);

        unwrap_all! {
            w.write(EventBuilder::start_element("hello"));
            w.write(EventBuilder::start_element("world"));
            w.write(EventBuilder::end_element());
            w.write(EventBuilder::end_element())
        }
    }

    assert_eq!(str::from_utf8(&b).unwrap(), r#"<hello><world></world></hello>"#);
}

#[test]
fn writing_comments_with_indentation() {
    let mut b = Vec::new();

    {
        let mut w = WriterConfig::new()
            .write_document_declaration(false)
            .perform_indent(true)
            .create_writer(&mut b);

        unwrap_all! {
            w.write(EventBuilder::start_element("hello"));
            w.write(EventBuilder::start_element("world"));
            w.write(Event::comment("  this is a manually padded comment\t"));
            w.write(Event::comment("this is an unpadded comment"));
            w.write(EventBuilder::end_element());
            w.write(EventBuilder::end_element())
        }
    }

    assert_eq!(
        str::from_utf8(&b).unwrap(),
        "<hello>
  <world>
    <!--  this is a manually padded comment\t-->
    <!-- this is an unpadded comment -->
  </world>
</hello>"
    );
}

#[test]
fn issue_112_overriding_namepace_prefix() {
    let mut b = Vec::new();

    {
        let mut w = WriterConfig::new()
            .write_document_declaration(false)
            .create_writer(&mut b);

        unwrap_all! {
            w.write(EventBuilder::start_element("iq").ns("", "jabber:client").ns("a", "urn:A"));
            w.write(EventBuilder::start_element("bind").ns("", "urn:ietf:params:xml:ns:xmpp-bind"));
            w.write(EventBuilder::end_element());
            w.write(EventBuilder::start_element("whatever").ns("a", "urn:X"));
            w.write(EventBuilder::end_element());
            w.write(EventBuilder::end_element())
        }
    }

    assert_eq!(
        str::from_utf8(&b).unwrap(),
        r#"<iq xmlns="jabber:client" xmlns:a="urn:A"><bind xmlns="urn:ietf:params:xml:ns:xmpp-bind" /><whatever xmlns:a="urn:X" /></iq>"#
    )
}

#[test]
fn attribute_escaping() {
    let mut b = Vec::new();

    {
        let mut w = WriterConfig::new()
            .write_document_declaration(false)
            .perform_indent(true)
            .create_writer(&mut b);

        unwrap_all! {
            w.write(EventBuilder::start_element("hello").attr("testLt", "<").attr("testGt", ">"));
            w.write(EventBuilder::end_element());
            w.write(EventBuilder::start_element("hello").attr("testQuot", "\"").attr("testApos", "\'"));
            w.write(EventBuilder::end_element());
            w.write(EventBuilder::start_element("hello").attr("testAmp", "&"));
            w.write(EventBuilder::end_element());
            w.write(EventBuilder::start_element("hello").attr("testNl", "\n").attr("testCr", "\r"));
            w.write(EventBuilder::end_element());
            w.write(EventBuilder::start_element("hello").attr("testNl", "\\n").attr("testCr", "\\r"));
            w.write(EventBuilder::end_element())
        }
    }
    assert_eq!(
        str::from_utf8(&b).unwrap(),
        "<hello testLt=\"&lt;\" testGt=\"&gt;\" />
<hello testQuot=\"&quot;\" testApos=\"&apos;\" />
<hello testAmp=\"&amp;\" />
<hello testNl=\"&#xA;\" testCr=\"&#xD;\" />
<hello testNl=\"\\n\" testCr=\"\\r\" />"
    );
}
