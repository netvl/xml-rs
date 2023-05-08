use std::fs::File;
use std::io::BufReader;
use xml::common::Position;
use xml::reader::*;

fn main() {
    let file_path = std::env::args_os().nth(1).expect("Please specify a path to an XML file");
    let file = File::open(file_path).unwrap();

    let mut reader = ParserConfig::default()
        .ignore_root_level_whitespace(false)
        .create_reader(BufReader::new(file));

    loop {
        match reader.next() {
            Ok(e) => {
                print!("{}\t", reader.position());

                match e {
                    XmlEvent::StartDocument { version, encoding, .. } => {
                        println!("StartDocument({version}, {encoding})")
                    },
                    XmlEvent::EndDocument => {
                        println!("EndDocument");
                        break;
                    }
                    XmlEvent::ProcessingInstruction { name, data } => {
                        println!("ProcessingInstruction({name}={:?})", data.as_deref().unwrap_or_default())
                    },
                    XmlEvent::StartElement { name, attributes, .. } => {
                        if attributes.is_empty() {
                            println!("StartElement({name})")
                        } else {
                            let attrs: Vec<_> = attributes
                                .iter()
                                .map(|a| format!("{}={:?}", &a.name, a.value))
                                .collect();
                            println!("StartElement({name} [{}])", attrs.join(", "))
                        }
                    }
                    XmlEvent::EndElement { name } => {
                        println!("EndElement({name})")
                    },
                    XmlEvent::Comment(data) => {
                        println!(r#"Comment("{}")"#, data.escape_debug())
                    }
                    XmlEvent::CData(data) => println!(r#"CData("{}")"#, data.escape_debug()),
                    XmlEvent::Characters(data) => {
                        println!(r#"Characters("{}")"#, data.escape_debug())
                    }
                    XmlEvent::Whitespace(data) => {
                        println!(r#"Whitespace("{}")"#, data.escape_debug())
                    }
                }
            }
            Err(e) => {
                eprintln!("Error at {}: {e}", reader.position());
                break;
            },
        }
    }
}
