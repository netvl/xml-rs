#![forbid(unsafe_code)]

use xml::reader::ParserConfig2;
use std::fmt;
use std::fs::File;
use std::io::{stderr, BufRead, BufReader, Write};
use std::path::Path;

use xml::common::Position;
use xml::name::OwnedName;
use xml::reader::{EventReader, ParserConfig, Result, XmlEvent};

/// Dummy function that opens a file, parses it, and returns a `Result`.
/// There can be IO errors (from `File::open`) and XML errors (from the parser).
/// Having `impl From<std::io::Error> for xml::reader::Error` allows the user to
/// do this without defining their own error type.
#[allow(dead_code)]
fn count_event_in_file(name: &Path) -> Result<usize> {
    let mut event_count = 0;
    for event in EventReader::new(BufReader::new(File::open(name)?)) {
        event?;
        event_count += 1;
    }
    Ok(event_count)
}

#[test]
fn issue_177() {
    assert!(xml::EventReader::from_str(";<?").next().is_err());
}

#[test]
fn issue_204() {
    xml::EventReader::from_str("<!DOCTYPE<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\
    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<").into_iter().for_each(|_| {});
}

#[test]
fn issue_227() {
    xml::EventReader::new_with_config(r#"<root>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
        <item><![CDATA[]]></item>
    </root>"#.as_bytes(),
    ParserConfig::new().cdata_to_characters(true)).into_iter().for_each(|_| {});
}

#[test]
fn sample_1_short() {
    test_files(
        "documents/sample_1.xml",
        "documents/sample_1_short.txt",
        ParserConfig::new()
            .ignore_comments(true)
            .whitespace_to_characters(true)
            .cdata_to_characters(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        false,
    );
}

#[test]
fn sample_1_full() {
    test_files(
        "documents/sample_1.xml",
        "documents/sample_1_full.txt",
        ParserConfig::new()
            .ignore_comments(false)
            .whitespace_to_characters(false)
            .cdata_to_characters(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false,
    );
}

#[test]
fn sample_2_short() {
    test_files(
        "documents/sample_2.xml",
        "documents/sample_2_short.txt",
        ParserConfig::new()
            .ignore_comments(true)
            .whitespace_to_characters(true)
            .cdata_to_characters(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        false,
    );
}

#[test]
fn sample_2_full() {
    test_files(
        "documents/sample_2.xml",
        "documents/sample_2_full.txt",
        ParserConfig::new()
            .ignore_comments(false)
            .whitespace_to_characters(false)
            .cdata_to_characters(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false,
    );
}

#[test]
fn sample_3_short() {
    test_files(
        "documents/sample_3.xml",
        "documents/sample_3_short.txt",
        ParserConfig::new()
            .ignore_comments(true)
            .whitespace_to_characters(true)
            .cdata_to_characters(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        true,
    );
}

#[test]
fn sample_3_full() {
    test_files(
        "documents/sample_3.xml",
        "documents/sample_3_full.txt",
        ParserConfig::new()
            .ignore_comments(false)
            .whitespace_to_characters(false)
            .cdata_to_characters(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        true,
    );
}

#[test]
fn sample_4_short() {
    test_files(
        "documents/sample_4.xml",
        "documents/sample_4_short.txt",
        ParserConfig::new()
            .ignore_comments(true)
            .whitespace_to_characters(true)
            .cdata_to_characters(true)
            .trim_whitespace(true)
            .coalesce_characters(true),
        false,
    );
}

#[test]
fn sample_4_full() {
    test_files(
        "documents/sample_4.xml",
        "documents/sample_4_full.txt",
        ParserConfig::new()
            .ignore_comments(false)
            .whitespace_to_characters(false)
            .cdata_to_characters(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false,
    );
}

#[test]
fn sample_5_short() {
    test_files(
        "documents/sample_5.xml",
        "documents/sample_5_short.txt",
        ParserConfig::new()
            .ignore_comments(true)
            .whitespace_to_characters(true)
            .cdata_to_characters(true)
            .trim_whitespace(true)
            .coalesce_characters(true)
            .add_entity("nbsp", " ")
            .add_entity("copy", "©")
            .add_entity("NotEqualTilde", "≂̸"),
        false,
    );
}

#[test]
fn sample_6_full() {
    test_files(
        "documents/sample_6.xml",
        "documents/sample_6_full.txt",
        ParserConfig::new()
            .ignore_root_level_whitespace(false)
            .ignore_comments(false)
            .whitespace_to_characters(false)
            .cdata_to_characters(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false,
    );
}

#[test]
fn sample_7() {
    test_files(
        "documents/sample_7.xml",
        "documents/sample_7_full.txt",
        ParserConfig::new()
            .ignore_root_level_whitespace(false)
            .ignore_comments(false)
            .whitespace_to_characters(false)
            .cdata_to_characters(false)
            .trim_whitespace(false)
            .coalesce_characters(false),
        false,
    );
}

#[test]
fn eof_1() {
    test(
        br#"<"#,
        br#"1:2 Unexpected end of stream"#,
        ParserConfig::new(),
        false,
    );
    test(
        br#"<?"#,
        br#"1:3 Unexpected end of stream"#,
        ParserConfig::new(),
        false,
    );
    test(
        br#"<?x"#,
        br#"1:4 Unexpected end of stream"#,
        ParserConfig::new(),
        false,
    );
    test(
        br#"<?xml"#,
        br#"1:6 Unexpected end of stream"#,
        ParserConfig::new(),
        false,
    );
    test(
        br#"<?xml v"#,
        br#"1:8 Unexpected end of stream"#,
        ParserConfig::new(),
        false,
    );
    test(
        br#"<?xml v?"#,
        br#"1:9 Unexpected end of stream"#,
        ParserConfig::new(),
        false,
    );
}

#[test]
fn limits1() {
    test(
        br#"<a attr='veeeeeeeeeeeeeeeeeeeerylooooooooooooooong'> "#,
        br#"
            |StartDocument(1.0, UTF-8)
            |1:14 This document is larger/more complex than allowed by the parser's configuration
        "#,
        ParserConfig::new().max_attribute_length(3),
        false,
    );
}

#[test]
fn limits2() {
    test(
        br#"<a a1='1' a2='2' a3='3' a4='4' a5='5'> "#,
        br#"
            |StartDocument(1.0, UTF-8)
            |1:30 This document is larger/more complex than allowed by the parser's configuration
        "#,
        ParserConfig::new().max_attributes(3),
        false,
    );
    test(
        br#"<a>veeeeeeeeeeeeeeeeeeeerylooooooooooooooong</a>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(a)
            |1:15 This document is larger/more complex than allowed by the parser's configuration
        "#,
        ParserConfig::new().max_data_length(10),
        false,
    );
}

#[test]
fn limits3() {
    test(
        br#"<a><!--veeeeeeeeeeeeeeeeeeeerylooooooooooooooong--></a>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(a)
            |1:19 This document is larger/more complex than allowed by the parser's configuration
        "#,
        ParserConfig::new().max_data_length(10).ignore_comments(false),
        false,
    );
    test(
        br#"<a><?ok --veeeeeeeeeeeeeeeeeeeerylooooooooooooooong--> ?></a>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(a)
            |1:20 This document is larger/more complex than allowed by the parser's configuration
        "#,
        ParserConfig::new().max_data_length(10),
        false,
    );
}

#[test]
fn limits4() {
    test(
        br#"<ok><averylongnamewat /></ok>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(ok)
            |1:10 This document is larger/more complex than allowed by the parser's configuration
        "#,
        ParserConfig::new().max_name_length(3),
        false,
    );
    test(
        br#"<a veeeeeeeeeeeeeeeeeeeerylooooooooooooooong='1'>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |1:15 This document is larger/more complex than allowed by the parser's configuration
        "#,
        ParserConfig::new().max_name_length(10),
        false,
    );
}

#[test]
fn bad_1() {
    test(
        br#"<?xml&.,"#,
        br#"1:6 Unexpected token inside processing instruction: <?xml&"#,
        ParserConfig::new(),
        false,
    );
}

#[test]
fn dashes_in_comments() {
    test(
        br#"<!-- comment -- --><hello/>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |1:14 Unexpected token '--' before ' '
        "#,
        ParserConfig::new(),
        false,
    );

    test(
        br#"<!-- comment ---><hello/>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |1:14 Unexpected token '--' before '-'
        "#,
        ParserConfig::new(),
        false,
    );
}

#[test]
fn tabs_1() {
    test(
        b"\t<a>\t<b/></a>",
        br#"
            |1:1 StartDocument(1.0, UTF-8)
            |1:2 StartElement(a)
            |1:6 StartElement(b)
            |1:6 EndElement(b)
            |1:10 EndElement(a)
            |1:14 EndDocument
        "#,
        ParserConfig::new().trim_whitespace(true),
        true,
    );
}

#[test]
fn issue_32_unescaped_cdata_end() {
    test(
        br#"<hello>]]></hello>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |1:8 Unexpected token: ]]>
        "#,
        ParserConfig::new(),
        false,
    );
}

#[test]
fn issue_unescaped_processing_instruction_end() {
    test(
        br#"<hello>?></hello>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |Characters("?>")
            |EndElement(hello)
            |EndDocument
        "#,
        ParserConfig::new(),
        false,
    );
}

#[test]
fn issue_unescaped_empty_tag_end() {
    test(
        br#"<hello>/></hello>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |Characters("/>")
            |EndElement(hello)
            |EndDocument
        "#,
        ParserConfig::new(),
        false,
    );
}

#[test]
fn issue_83_duplicate_attributes() {
    test(
        br#"<hello><some-tag a='10' a="20"></hello>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(hello)
            |1:26 Attribute 'a' is redefined
        "#,
        ParserConfig::new(),
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
        "#.as_bytes(),  // FIXME: it shouldn't be 10, looks like indices are off slightly
        ParserConfig::new(),
        false,
    );
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
        ParserConfig::new(),
        false,
    );
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
        ParserConfig::new(),
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
        ParserConfig::new(),
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
        ParserConfig::new(),
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
        ParserConfig::new(),
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
        ParserConfig::new(),
        false,
    );
}

#[test]
fn error_after_end() {
    test(
        br#"<!-- should fail -->
<foo>
</foo>
&lt;"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(foo)
            |Whitespace("\n")
            |EndElement(foo)
            |4:1 Unexpected token: &
        "#,
        ParserConfig::new().ignore_root_level_whitespace(true),
        false,
    );
}

#[test]
fn issue_replacement_character_entity_reference() {
    test(
        br#"<doc>&#55357;&#56628;</doc>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(doc)
            |1:13 Invalid character U+D83D
        "#,
        ParserConfig::new(),
        false,
    );

    test(
        br#"<doc>&#xd83d;&#xdd34;</doc>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(doc)
            |1:13 Invalid character U+D83D
        "#,
        ParserConfig::new(),
        false,
    );

    test(
        br#"<doc>&#55357;&#56628;</doc>"#,
        format!(
            r#"
                |StartDocument(1.0, UTF-8)
                |StartElement(doc)
                |Characters("{replacement_character}{replacement_character}")
                |EndElement(doc)
                |EndDocument
            "#,
            replacement_character = "\u{fffd}"
        )
        .as_bytes(),
        ParserConfig::new()
            .replace_unknown_entity_references(true),
        false,
    );

    test(
        br#"<doc>&#xd83d;&#xdd34;</doc>"#,
        format!(
            r#"
                |StartDocument(1.0, UTF-8)
                |StartElement(doc)
                |Characters("{replacement_character}{replacement_character}")
                |EndElement(doc)
                |EndDocument
            "#,
            replacement_character = "\u{fffd}"
        )
        .as_bytes(),
        ParserConfig::new()
            .replace_unknown_entity_references(true),
        false,
    );
}

#[test]
fn issue_replacement_character_control_character() {
    test(
        br#"<doc>&#16;</doc>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(doc)
            |1:10 Invalid character U+0010
        "#,
        ParserConfig::new(),
        false,
    );

    test(
        br#"<doc>&#x10;</doc>"#,
        br#"
            |StartDocument(1.0, UTF-8)
            |StartElement(doc)
            |1:11 Invalid character U+0010
        "#,
        ParserConfig::new(),
        false,
    );

    test(
        br#"<doc>&#16;</doc>"#,
        format!(
            r#"
                |StartDocument(1.0, UTF-8)
                |StartElement(doc)
                |Characters("{replacement_character}")
                |EndElement(doc)
                |EndDocument
            "#,
            replacement_character = "\u{fffd}"
        )
        .as_bytes(),
        ParserConfig::new()
            .replace_unknown_entity_references(true),
        false,
    );

    test(
        br#"<doc>&#x10;</doc>"#,
        format!(
            r#"
                |StartDocument(1.0, UTF-8)
                |StartElement(doc)
                |Characters("{replacement_character}")
                |EndElement(doc)
                |EndDocument
            "#,
            replacement_character = "\u{fffd}"
        )
        .as_bytes(),
        ParserConfig::new()
            .replace_unknown_entity_references(true),
        false,
    );
}

#[test]
fn push_pos_issue() {
    let source = "<n><!---->L<!----><!----><!----><!----><!----><!----><!----><!----><!---->\"<!----><!---->L<!----><!----></n>";
    let parser = ParserConfig::new()
        .cdata_to_characters(true)
        .ignore_comments(true)
        .coalesce_characters(false)
        .create_reader(std::io::Cursor::new(source));
    parser.into_iter().for_each(|e| { e.unwrap(); });
}

// clones a lot but that's fine
fn trim_until_bar(s: String) -> String {
    match s.trim() {
        ts if ts.starts_with('|') => return ts[1..].to_owned(),
        _ => {}
    }
    s
}

#[track_caller]
fn test_files(input_path: &str, output_path: &str, config: ParserConfig, test_position: bool) {
    let input = std::fs::read(Path::new("tests").join(input_path)).unwrap();
    let output = std::fs::read(Path::new("tests").join(output_path)).unwrap();
    let should_print = std::env::var("PRINT_SPEC").map_or(false, |val| val == "1");
    let mut out = if should_print { Some(vec![]) } else { None };

    test_inner(&input, &output, config.into(), test_position, out.as_mut());

    if let Some(out) = out {
        std::fs::write(Path::new("tests").join(output_path), out).unwrap();
    }
}

#[track_caller]
fn test(input: &[u8], output: &[u8], config: impl Into<ParserConfig2>, test_position: bool) {
    let should_print = std::env::var("PRINT_SPEC").map_or(false, |val| val == "1");
    let mut out = if should_print { Some(vec![]) } else { None };

    test_inner(input, output, config.into(), test_position, out.as_mut());

    if let Some(out) = out {
        stderr().write_all(&out).unwrap();
    }
}

#[track_caller]
fn test_inner(input: &[u8], output: &[u8], config: ParserConfig2, test_position: bool, mut out: Option<&mut Vec<u8>>) {
    let mut reader = config.create_reader(input);
    let mut spec_lines = BufReader::new(output).lines()
        .map(std::result::Result::unwrap)
        .enumerate()
        .map(|(i, line)| (i, trim_until_bar(line)))
        .filter(|(_, line)| !line.trim().is_empty());

    loop {
        let e = reader.next();
        let line = if test_position {
            format!("{} {}", reader.position(), Event(&e))
        } else {
            format!("{}", Event(&e))
        };

        if let Some(out) = out.as_mut() {
            writeln!(**out, "{line}").unwrap();
        } else if let Some((n, spec)) = spec_lines.next() {
            if line != spec {
                const SPLITTER: &str = "-------------------";
                panic!("\n{}\nUnexpected event at line {}:\nExpected: {}\nFound:    {}\n{}\n",
                       SPLITTER, n + 1, spec, line, std::str::from_utf8(output).unwrap());
            }
        } else {
            panic!("Unexpected event: {line}");
        }

        match e {
            Ok(XmlEvent::EndDocument) | Err(_) => break,
            _ => {},
        }
    }
}

#[test]
fn skip() {
    let mut reader = xml::EventReader::from_str(r#"<a><x><c foo="bar"><d bar="none" /></c></x>Hello</a>"#);

    'outer: loop {
        match reader.next().expect("Unexpected error!") {
            XmlEvent::StartElement { name, .. } => {
                if name.local_name == "x" {
                    reader.skip().expect("Unexpected error during skip()");
                    // After the skip, we should see "Hello" chars and
                    // the </a> end element
                    assert_eq!(Ok(XmlEvent::Characters("Hello".to_string())), reader.next());
                    assert_eq!(Ok(XmlEvent::EndElement{ name: OwnedName::local("a".to_string()) }), reader.next());
                    assert_eq!(Ok(XmlEvent::EndDocument), reader.next());
                    break 'outer;
                } else {
                    // Should never see the "c" element, since it should be skipped!
                    assert_ne!(name.local_name, "c");
                    // Should never see the "d" element, since it should be skipped!
                    assert_ne!(name.local_name, "d");
                }
            }
            XmlEvent::EndElement { name, .. } => {
                // Should never see the "c" element, since it should be skipped!
                assert_ne!(name.local_name, "c");
                // Should never see the "d" element, since it should be skipped!
                assert_ne!(name.local_name, "d");
                // Should never see the "x" end element, should also be skipped!
                assert_ne!(name.local_name, "x");
            }
            XmlEvent::EndDocument => unreachable!("Should not be reached in this test!"),
            _ => {},
        }
    }
}

// Here we define our own string representation of events so we don't depend
// on the specifics of Display implementation for XmlEvent and OwnedName.

struct Name<'a>(&'a OwnedName);

impl<'a> fmt::Display for Name<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref namespace) = self.0.namespace {
            write!(f, "{{{namespace}}}")?;
        }

        if let Some(ref prefix) = self.0.prefix {
            write!(f, "{prefix}:")?;
        }

        f.write_str(&self.0.local_name)
    }
}

struct Event<'a>(&'a Result<XmlEvent>);

impl<'a> fmt::Display for Event<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let empty = String::new();
        match *self.0 {
            Ok(ref e) => match *e {
                XmlEvent::StartDocument { ref version, ref encoding, .. } =>
                    write!(f, "StartDocument({version}, {encoding})"),
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
                        write!(f, "StartElement({} [{}])", Name(name), attrs.join(", "))
                    }
                },
                XmlEvent::EndElement { ref name } =>
                    write!(f, "EndElement({})", Name(name)),
                XmlEvent::Comment(ref data) =>
                    write!(f, r#"Comment("{}")"#, data.escape_debug()),
                XmlEvent::CData(ref data) =>
                    write!(f, r#"CData("{}")"#, data.escape_debug()),
                XmlEvent::Characters(ref data) =>
                    write!(f, r#"Characters("{}")"#, data.escape_debug()),
                XmlEvent::Whitespace(ref data) =>
                    write!(f, r#"Whitespace("{}")"#, data.escape_debug()),
            },
            Err(ref e) => e.fmt(f),
        }
    }
}
