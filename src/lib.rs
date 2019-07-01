//#![warn(missing_doc)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![forbid(non_camel_case_types)]
#![type_length_limit="988530600"]

//! This crate currently provides an almost XML 1.0/1.1-compliant pull parser.

extern crate failure;
#[macro_use]
extern crate failure_derive;

#[cfg(test)]
extern crate encoding;
#[cfg(test)]
extern crate quickcheck;

pub use encoding_rs;

pub use self::reader::EventReader;
pub use self::reader::ParserConfig;
pub use self::writer::EmitterConfig;
pub use self::writer::EventWriter;

pub mod attribute;
pub mod chars;
pub mod event;
pub mod macros;
pub mod name;
pub mod namespace;
pub mod position;
pub mod reader;
pub mod util;
pub mod writer;

pub mod attribute2;
pub mod name2;

pub mod reader3;
