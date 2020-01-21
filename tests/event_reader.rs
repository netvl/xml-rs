use std::env;
use std::fmt;
use std::io::{stderr, BufRead, BufReader, Cursor, Write};

use xml::reader::Result;
use xml::{Position, ReaderConfig};

#[test]
fn sample_1_short() {
    test(
        include_bytes!("documents/sample_1.xml"),
        include_bytes!("documents/sample_1_short.txt"),
        ReaderConfig::new()
            .ignore_comments(true)
            .whitespace_to_text(true)
            .cdata_to_text(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        false,
    );
}

#[test]
fn sample_1_full() {
    test(
        include_bytes!("documents/sample_1.xml"),
        include_bytes!("documents/sample_1_full.txt"),
        ReaderConfig::new()
            .ignore_comments(false)
            .whitespace_to_text(false)
            .cdata_to_text(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false,
    );
}

#[test]
fn sample_2_short() {
    test(
        include_bytes!("documents/sample_2.xml"),
        include_bytes!("documents/sample_2_short.txt"),
        ReaderConfig::new()
            .ignore_comments(true)
            .whitespace_to_text(true)
            .cdata_to_text(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        false,
    );
}

#[test]
fn sample_2_full() {
    test(
        include_bytes!("documents/sample_2.xml"),
        include_bytes!("documents/sample_2_full.txt"),
        ReaderConfig::new()
            .ignore_comments(false)
            .whitespace_to_text(false)
            .cdata_to_text(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false,
    );
}

#[test]
fn sample_3_short() {
    test(
        include_bytes!("documents/sample_3.xml"),
        include_bytes!("documents/sample_3_short.txt"),
        ReaderConfig::new()
            .ignore_comments(true)
            .whitespace_to_text(true)
            .cdata_to_text(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        true,
    );
}

#[test]
fn sample_3_full() {
    test(
        include_bytes!("documents/sample_3.xml"),
        include_bytes!("documents/sample_3_full.txt"),
        ReaderConfig::new()
            .ignore_comments(false)
            .whitespace_to_text(false)
            .cdata_to_text(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        true,
    );
}

#[test]
fn sample_4_short() {
    test(
        include_bytes!("documents/sample_4.xml"),
        include_bytes!("documents/sample_4_short.txt"),
        ReaderConfig::new()
            .ignore_comments(true)
            .whitespace_to_text(true)
            .cdata_to_text(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        false,
    );
}

#[test]
fn sample_4_full() {
    test(
        include_bytes!("documents/sample_4.xml"),
        include_bytes!("documents/sample_4_full.txt"),
        ReaderConfig::new()
            .ignore_comments(false)
            .whitespace_to_text(false)
            .cdata_to_text(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false,
    );
}

#[test]
fn sample_5_short() {
    test(
        include_bytes!("documents/sample_5.xml"),
        include_bytes!("documents/sample_5_short.txt"),
        ReaderConfig::new()
            .ignore_comments(true)
            .whitespace_to_text(true)
            .cdata_to_text(true)
            .trim_whitespace(true)
            .coalesce_characters(true)
            .add_entity("nbsp", " ")
            .add_entity("copy", "©")
            .add_entity("NotEqualTilde", "≂̸"),
        false,
    );
}

#[test]
fn eof_1() {
    test(
        br#"<?xml"#,
        br#"1:6 Unexpected end of stream: no root element found"#,
        ReaderConfig::new(),
        false,
    );
}

#[test]
fn bad_1() {
    test(
        br#"<?xml&.,"#,
        br#"1:6 Unexpected token: <?xml&"#,
        ReaderConfig::new(),
        false,
    );
}

#[test]
fn dashes_in_comments() {
    test(
        br#"<!-- comment -- --><hello/>"#,
        br#"
            |1:14 Unexpected token '--' before ' '
        "#,
        ReaderConfig::new(),
        false,
    );

    test(
        br#"<!-- comment ---><hello/>"#,
        br#"
            |1:14 Unexpected token '--' before '-'
        "#,
        ReaderConfig::new(),
        false,
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
        ReaderConfig::new().trim_whitespace(true),
        true,
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
        ReaderConfig::new(),
        false,
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
        "#
        .as_bytes(), // FIXME: it shouldn't be 10, looks like indices are off slightly
        ReaderConfig::new(),
        false,
    )
}

#[test]
fn issue_98_cdata_ending_with_right_bracket() {
    test(
        br#"<hello><![CDATA[Foo [Bar]]]></hello>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |CData("Foo [Bar]")
            |EndElement(hello)
            |EndDocument
        "#,
        ReaderConfig::new(),
        false,
    )
}

#[test]
fn issue_105_unexpected_double_dash() {
    test(
        br#"<hello>-- </hello>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |Characters("-- ")
            |EndElement(hello)
            |EndDocument
        "#,
        ReaderConfig::new(),
        false,
    );

    test(
        br#"<hello>--</hello>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |Characters("--")
            |EndElement(hello)
            |EndDocument
        "#,
        ReaderConfig::new(),
        false,
    );

    test(
        br#"<hello>--></hello>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |Characters("-->")
            |EndElement(hello)
            |EndDocument
        "#,
        ReaderConfig::new(),
        false,
    );

    test(
        br#"<hello><![CDATA[--]]></hello>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |CData("--")
            |EndElement(hello)
            |EndDocument
        "#,
        ReaderConfig::new(),
        false,
    );
}

#[test]
fn issue_attribues_have_no_default_namespace() {
    test(
        br#"<hello xmlns="urn:foo" x="y"/>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement({urn:foo}hello [x="y"])
            |EndElement({urn:foo}hello)
            |EndDocument
        "#,
        ReaderConfig::new(),
        false,
    );
}

// clones a lot but that's fine
fn trim_until_bar(s: String) -> String {
    match s.trim() {
        ts if ts.starts_with('|') => return ts[1..].to_owned(),
        _ => {}
    }
    s
}

// If PRINT_SPEC env variable is set, print the lines
// to stderr instead of comparing with the output
// it can be used like this:
// PRINT_SPEC=1 cargo test --test event_reader sample_1_full 2> sample_1_full.txt
fn print_spec_enabled() -> bool {
    let result = env::var("PRINT_SPEC");
    match result.as_ref().map(|s| s.as_str()) {
        Ok("1") | Ok("true") | Ok("y") | Ok("yes") => true,
        _ => false,
    }
}

fn test(input: &[u8], expected_output: &[u8], config: ReaderConfig, test_position: bool) {
    let mut reader = config.create_reader(Cursor::new(input));
    let mut spec_lines = BufReader::new(expected_output)
        .lines()
        .map(|line| line.unwrap())
        .enumerate()
        .map(|(i, line)| (i, trim_until_bar(line)))
        .filter(|&(_, ref line)| !line.trim().is_empty());

    loop {
        let e = reader.advance().map(|_| reader.take_current().unwrap());
        let line = if test_position {
            format!("{} {}", reader.position(), Event(&e))
        } else {
            format!("{}", Event(&e))
        };

        if print_spec_enabled() {
            writeln!(&mut stderr(), "{}", line).unwrap();
        } else {
            if let Some((n, spec)) = spec_lines.next() {
                if line != spec {
                    const SPLITTER: &'static str = "-------------------";
                    panic!(
                        "\n{}\nUnexpected event at line {}:\nExpected: {}\nFound:    {}\n{}\n",
                        SPLITTER,
                        n + 1,
                        spec,
                        line,
                        SPLITTER
                    );
                }
            } else {
                panic!("Unexpected event: {}", line);
            }
        }

        match e {
            Ok(xml::Event::EndDocument) | Err(_) => break,
            _ => {}
        }
    }
}

// Here we define our own string representation of events so we don't depend
// on the specifics of Display implementation for xml::Event and OwnedName.

struct Name<'r, 'a>(&'r xml::name::Name<'a>);

impl<'r, 'a> fmt::Display for Name<'r, 'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref namespace) = self.0.namespace {
            write!(f, "{{{}}}", namespace)?;
        }

        if let Some(ref prefix) = self.0.prefix {
            write!(f, "{}:", prefix)?;
        }

        write!(f, "{}", self.0.local_name)
    }
}

struct Event<'r, 'a>(&'r Result<xml::Event<'a>>);

impl<'r, 'a> fmt::Display for Event<'r, 'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            Ok(ref e) => match *e {
                xml::Event::StartDocument {
                    ref version,
                    ref encoding,
                    ..
                } => write!(f, "StartDocument({}, {})", version, encoding),
                xml::Event::DoctypeDeclaration { ref content } => write!(f, "DoctypeDeclaration({:?})", content),
                xml::Event::EndDocument => write!(f, "EndDocument"),
                xml::Event::ProcessingInstruction { ref name, ref data } => {
                    write!(f, "ProcessingInstruction({}={:?})", name, data.as_deref().unwrap_or(""))
                }
                xml::Event::StartElement {
                    ref name,
                    ref attributes,
                    ..
                } => {
                    if attributes.is_empty() {
                        write!(f, "StartElement({})", Name(name))
                    } else {
                        let attrs: Vec<_> = attributes
                            .iter()
                            .map(|a| format!("{}={:?}", Name(&a.name), a.value))
                            .collect();
                        write!(f, "StartElement({} [{}])", Name(name), attrs.join(", "))
                    }
                }
                xml::Event::EndElement { ref name } => write!(f, "EndElement({})", Name(name)),
                xml::Event::Comment(ref data) => write!(f, "Comment({:?})", data),
                xml::Event::CData(ref data) => write!(f, "CData({:?})", data),
                xml::Event::Text(ref data) => write!(f, "Characters({:?})", data),
                xml::Event::Whitespace(ref data) => write!(f, "Whitespace({:?})", data),
            },
            Err(ref e) => e.fmt(f),
        }
    }
}
