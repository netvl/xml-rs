extern crate xml;

use std::env;
use std::fmt;
use std::io::{BufRead, BufReader, Write, stderr};
use std::sync::{Once, ONCE_INIT};
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
            .coalesce_characters(true)
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
            .coalesce_characters(false)
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
            .coalesce_characters(true)
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
            .coalesce_characters(false)
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
            .coalesce_characters(true)
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
            .coalesce_characters(false)
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
            .coalesce_characters(true)
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
            .coalesce_characters(false)
    );

}

static START: Once = ONCE_INIT;
static mut PRINT: bool = false;

fn test(input: &[u8], output: &[u8], config: ParserConfig) {
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
    let mut spec_lines = BufReader::new(output).lines().enumerate();
    loop {
        let e = reader.next();
        let line = format!("{}", Event(&e));

        if unsafe { PRINT } {
            let _ = stderr().write(format!("{}\n", line).as_bytes());
        }
        else {
            if let Some((n, Ok(spec))) = spec_lines.next() {
                if line != spec {
                    panic!("Unexpected event at line {}:\nExpected: {}\nFound: {}",
                           n + 1, spec, line);
                }
            }
            else {
                panic!("Unexpected event: {}", line);
            }
        }

        match e {
            XmlEvent::Error(_) | XmlEvent::EndDocument => break,
            _ => {},
        }
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
                    write!(f, "StartElement({})", name)
                }
                else {
                    let attrs: Vec<_> = attributes.iter()
                        .map(|a| format!("{}={:?}", a.name, a.value)) .collect();
                    write!(f, "StartElement({} [{}])", name, attrs.connect(", "))
                }
            },
            XmlEvent::EndElement { ref name } =>
                write!(f, "EndElement({})", name),
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
