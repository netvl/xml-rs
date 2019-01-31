//#![warn(missing_doc)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![forbid(non_camel_case_types)]
#![forbid(unsafe_code)]

//! This crate currently provides an almost XML 1.0/1.1-compliant pull parser.

#[macro_use]
extern crate failure;

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
extern crate encoding;

#[cfg(feature = "encodings")]
pub use encoding_rs;

#[cfg(doctest)]
#[macro_use]
extern crate doc_comment;

#[cfg(doctest)]
doctest!("../Readme.md");

pub use self::reader::EventReader;
pub use self::reader::ParserConfig;
pub use self::writer::EventWriter;
pub use self::writer::EmitterConfig;

pub mod macros;
pub mod name;
pub mod attribute;
pub mod namespace;
pub mod reader;
pub mod writer;
pub mod position;
pub mod util;
pub mod chars;
pub mod event;

pub mod reader2;
pub mod name2;
pub mod attribute2;
