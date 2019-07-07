extern crate failure;
extern crate xml;

use std::io::{self, Read, Cursor};

use failure::Fail;

use xml::reader::new_parser;

fn main() {
    let mut stdin = io::stdin();
    let mut data = Vec::new();
    stdin.read_to_end(&mut data).unwrap();

    let mut parser = new_parser(Cursor::new(data));

    loop {
        match parser.next() {
            Ok(e) => println!("Event: {:?}", e),
            Err(e) => {
                println!("Error:\n---\n{:#?}\n---\n{}", e, e.cause().unwrap());
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
