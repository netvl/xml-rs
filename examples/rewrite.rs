//! See <https://lib.rs/crates/svg-hush> for a real-world example.

use xml::EmitterConfig;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use xml::reader::{ParserConfig, Result};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let arg = std::env::args_os().nth(1);
    let file_path = Path::new(arg.as_deref().unwrap_or("tests/documents/sample_1.xml".as_ref()));
    let file = BufReader::new(File::open(file_path)
            .map_err(|e| format!("Can't open {}: {e}", file_path.display()))?);

    let mut reader = ParserConfig::default()
        .ignore_root_level_whitespace(true)
        .ignore_comments(false)
        .cdata_to_characters(true)
        .coalesce_characters(true)
        .create_reader(file);

    let stdout = std::io::stdout().lock();

    let mut writer = EmitterConfig::default()
        .create_writer(stdout);

    loop {
        let reader_event = reader.next()?;

        match reader_event {
            xml::reader::XmlEvent::EndDocument => break,
            xml::reader::XmlEvent::StartElement { name, mut attributes, namespace } => {
                let event = xml::writer::XmlEvent::StartElement  {
                    name: name.borrow(),
                    namespace: namespace.borrow(),
                    attributes: attributes.iter_mut().map(|attr| {
                        attr.value = alternating_caps(&attr.value);
                        attr.borrow()
                    }).collect(),
                };
                writer.write(event)?;
            },
            xml::reader::XmlEvent::Characters(text) => {
                let text = alternating_caps(&text);
                let event = xml::writer::XmlEvent::Characters(&text);
                writer.write(event)?;
            },
            xml::reader::XmlEvent::Comment(text) => {
                let text = alternating_caps(&text);
                let event = xml::writer::XmlEvent::Comment(&text);
                writer.write(event)?;
            },
            other => {
                if let Some(writer_event) = other.as_writer_event() {
                    writer.write(writer_event)?;
                }
            }
        }

    }
    Ok(())
}

fn alternating_caps(text: &str) -> String {
    text.chars().enumerate()
        .map(|(i, ch)| if i&1==0 { ch.to_ascii_uppercase() } else { ch.to_ascii_lowercase() })
        .collect()
}
