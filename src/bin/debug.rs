extern crate xml;

use std::io::{self, Read, Cursor};
use std::error::Error;

use xml::reader::ReaderConfig;
use xml::event::XmlEvent;

fn main() {
    let mut stdin = io::stdin();
    let mut data = Vec::new();
    stdin.read_to_end(&mut data).unwrap();

    let mut parser = ReaderConfig::default().reader_from_buf_read(Cursor::new(data));

    loop {
        match parser.next() {
            Ok(e) => {
                println!("Event: {:?}", e);
                if let XmlEvent::EndDocument = e {
                    break;
                }
            },
            Err(e) => {
                println!("Error:\n---\n{:#?}\n---\n{}", e, e.source().unwrap());
                break;
            }
        }
    }
//    let mut parser = ParserConfig::new().ignore_comments(false).create_parser(stdin.lock());
//    let mut buffer = Buffer::new();
//
//    loop {
//        match parser.next(&mut buffer) {
//            Ok(e) => println!("Event: {:?}", e),
//            Err(e) => {
//                println!("Error({:?}): {}", e, e.cause().unwrap());
//                break;
//            }
//        }
//    }
}
