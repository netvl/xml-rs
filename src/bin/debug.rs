extern crate xml;
extern crate failure;

use std::io;

use failure::Fail;

use xml::reader2::{ParserConfig, Buffer};

fn main() {
    let stdin = io::stdin();
    let mut parser = ParserConfig::new().ignore_comments(false).create_parser(stdin.lock());
    let mut buffer = Buffer::new();

    loop {
        match parser.next(&mut buffer) {
            Ok(e) => println!("Event: {:?}", e),
            Err(e) => {
                println!("Error({:?}): {}", e, e.cause().unwrap());
                break;
            },
        }
    }
}
