use std::io::{self, BufRead};

use encoding_rs::{self, Encoding};

use errors::*;

mod errors;
mod lexer;
mod config;
mod encodings;

fn detect_encoding<R: BufRead>(r: &mut R) -> Result<Option<&'static Encoding>> {
    let buf = r.fill_buf()?;

    if buf.len() < 2 {
        return Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "Unexpected end of stream when detecting encoding"
        ).into());
    }

    match Encoding::for_bom(buf) {
        Some((encoding_rs::UTF_8, _)) => 
    }
}
