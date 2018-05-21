extern crate xml;
extern crate failure;

use std::io;

use failure::Fail;

use xml::reader2::Parser;

fn main() {
    let stdin = io::stdin();
    let mut parser = Parser::new(stdin.lock());
    let mut buffer = String::new();

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
