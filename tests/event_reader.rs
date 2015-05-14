extern crate xml;

use std::env;
use std::fmt;
use std::io::{BufRead, BufReader, Write, stderr};
use std::sync::{Once, ONCE_INIT};
use xml::name::OwnedName;
use xml::common::Position;
use xml::reader::events::XmlEvent;
use xml::reader::{EventReader, ParserConfig};

#[test]
fn sample_1_short() {
    test(
        include_bytes!("event_reader/sample_1.xml"),
        include_bytes!("event_reader/sample_1_short.txt"),
        ParserConfig::new()
            .ignore_comments(true)
            .whitespace_to_characters(true)
            .cdata_to_characters(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        false
    );
}

#[test]
fn sample_1_full() {
    test(
        include_bytes!("event_reader/sample_1.xml"),
        include_bytes!("event_reader/sample_1_full.txt"),
        ParserConfig::new()
            .ignore_comments(false)
            .whitespace_to_characters(false)
            .cdata_to_characters(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false
    );
}

#[test]
fn sample_2_short() {
    test(
        include_bytes!("event_reader/sample_2.xml"),
        include_bytes!("event_reader/sample_2_short.txt"),
        ParserConfig::new()
            .ignore_comments(true)
            .whitespace_to_characters(true)
            .cdata_to_characters(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        false
    );
}

#[test]
fn sample_2_full() {
    test(
        include_bytes!("event_reader/sample_2.xml"),
        include_bytes!("event_reader/sample_2_full.txt"),
        ParserConfig::new()
            .ignore_comments(false)
            .whitespace_to_characters(false)
            .cdata_to_characters(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false
    );
}

#[test]
fn sample_3_short() {
    test(
        include_bytes!("event_reader/sample_3.xml"),
        include_bytes!("event_reader/sample_3_short.txt"),
        ParserConfig::new()
            .ignore_comments(true)
            .whitespace_to_characters(true)
            .cdata_to_characters(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        true
    );
}

#[test]
fn sample_3_full() {
    test(
        include_bytes!("event_reader/sample_3.xml"),
        include_bytes!("event_reader/sample_3_full.txt"),
        ParserConfig::new()
            .ignore_comments(false)
            .whitespace_to_characters(false)
            .cdata_to_characters(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        true
    );
}

#[test]
fn sample_4_short() {
    test(
        include_bytes!("event_reader/sample_4.xml"),
        include_bytes!("event_reader/sample_4_short.txt"),
        ParserConfig::new()
            .ignore_comments(true)
            .whitespace_to_characters(true)
            .cdata_to_characters(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        false
    );
}

#[test]
fn sample_4_full() {
    test(
        include_bytes!("event_reader/sample_4.xml"),
        include_bytes!("event_reader/sample_4_full.txt"),
        ParserConfig::new()
            .ignore_comments(false)
            .whitespace_to_characters(false)
            .cdata_to_characters(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false
    );

}

#[test]
fn eof_1() {
    test(
        br#"<?xml"#,
        br#"1:6 Unexpected end of stream: no root element found"#,
        ParserConfig::new(),
        false
    );
}

#[test]
fn bad_1() {
    test(
        br#"<?xml&.,"#,
        br#"1:6 Unexpected token: <?xml&"#,
        ParserConfig::new(),
        false
    );
}

#[test]
fn tabs_1() {
    test(
        b"\t<a>\t<b/></a>",
        br#"
            |1:2 StartDocument(1.0, UTF-8)
            |1:2 StartElement(a)
            |1:6 StartElement(b)
            |1:6 EndElement(b)
            |1:10 EndElement(a)
            |1:14 EndDocument
        "#,
        ParserConfig::new()
            .trim_whitespace(true),
        true
    );
}

#[test]
fn issue_83_duplicate_attributes() {
    test(
        br#"<hello><some-tag a='10' a="20"></hello>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |1:30 Attribute 'a' is redefined
        "#,
        ParserConfig::new(),
        false
    );
}

#[test]
fn issue_93_large_characters_in_entity_references() {
    test(
        r#"<hello>&𤶼;</hello>"#.as_bytes(),
        r#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |1:10 Unexpected entity: 𤶼
        "#.as_bytes(),  // TODO: it shouldn't be 10, looks like indices are off slightly
        ParserConfig::new(),
        false
    )
}

static START: Once = ONCE_INIT;
static mut PRINT: bool = false;

// clones a lot but that's fine
fn trim_until_bar(s: String) -> String {
    match s.trim() {
        ts if ts.starts_with('|') => return ts[1..].to_owned(),
        _ => {}
    }
    s
}

fn test(input: &[u8], output: &[u8], config: ParserConfig, test_position: bool) {
    // If PRINT_SPEC env variable is set, print the lines
    // to stderr instead of comparing with the output
    // it can be used like this:
    // PRINT_SPEC=1 cargo test --test event_reader sample_1_full 2> sample_1_full.txt
    START.call_once(|| {
        for (key, value) in env::vars() {
            if key == "PRINT_SPEC" && value == "1" {
                unsafe { PRINT = true; }
            }
        }
    });

    let mut reader = EventReader::with_config(input, config);
    let mut spec_lines = BufReader::new(output).lines()
        .map(|line| line.unwrap())
        .enumerate()
        .map(|(i, line)| (i, trim_until_bar(line)))
        .filter(|&(_, ref line)| !line.trim().is_empty());

    loop {
        let e = reader.next();
        let line =
            if test_position {
                format!("{} {}", reader.position(), Event(&e))
            } else {
                format!("{}", Event(&e))
            };

        if unsafe { PRINT } {
            writeln!(&mut stderr(), "{}", line).unwrap();
        } else {
            if let Some((n, spec)) = spec_lines.next() {
                if line != spec {
                    const SPLITTER: &'static str = "-------------------";
                    panic!("\n{}\nUnexpected event at line {}:\nExpected: {}\nFound:    {}\n{}\n",
                           SPLITTER, n + 1, spec, line, SPLITTER);
                }
            } else {
                panic!("Unexpected event: {}", line);
            }
        }

        match e {
            XmlEvent::Error(_) | XmlEvent::EndDocument => break,
            _ => {},
        }
    }
}

// Here we define our own string representation of events so we don't depend
// on the specifics of Display implementation for XmlEvent and OwnedName.

struct Name<'a>(&'a OwnedName);

impl <'a> fmt::Display for Name<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref namespace) = self.0.namespace {
            try! { write!(f, "{{{}}}", namespace) }
        }

        if let Some(ref prefix) = self.0.prefix {
            try! { write!(f, "{}:", prefix) }
        }

        write!(f, "{}", self.0.local_name)
    }
}

struct Event<'a>(&'a XmlEvent);

impl<'a> fmt::Display for Event<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let empty = String::new();
        match *self.0 {
            XmlEvent::StartDocument { ref version, ref encoding, .. } =>
                write!(f, "StartDocument({}, {})", version, encoding),
            XmlEvent::EndDocument =>
                write!(f, "EndDocument"),
            XmlEvent::ProcessingInstruction { ref name, ref data } =>
                write!(f, "ProcessingInstruction({}={:?})", name,
                    data.as_ref().unwrap_or(&empty)),
            XmlEvent::StartElement { ref name, ref attributes, .. } => {
                if attributes.is_empty() {
                    write!(f, "StartElement({})", Name(name))
                }
                else {
                    let attrs: Vec<_> = attributes.iter()
                        .map(|a| format!("{}={:?}", Name(&a.name), a.value)) .collect();
                    write!(f, "StartElement({} [{}])", Name(name), attrs.connect(", "))
                }
            },
            XmlEvent::EndElement { ref name } =>
                write!(f, "EndElement({})", Name(name)),
            XmlEvent::Comment(ref data) =>
                write!(f, "Comment({:?})", data),
            XmlEvent::CData(ref data) =>
                write!(f, "CData({:?})", data),
            XmlEvent::Characters(ref data) =>
                write!(f, "Characters({:?})", data),
            XmlEvent::Whitespace(ref data) =>
                write!(f, "Whitespace({:?})", data),
            XmlEvent::Error(ref e) => e.fmt(f),
        }
    }
}
