//#![warn(missing_doc)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![forbid(non_camel_case_types)]

//! This crate currently provides an almost XML 1.0/1.1-compliant pull parser.

#[macro_use]
extern crate failure;

#[cfg(feature = "encodings")]
pub extern crate encoding_rs;

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
extern crate encoding;

pub use reader::EventReader;
pub use reader::ParserConfig;
pub use writer::EventWriter;
pub use writer::EmitterConfig;

pub mod macros;
pub mod name;
pub mod attribute;
pub mod common;
pub mod namespace;
pub mod reader;
pub mod reader2;
pub mod writer;
pub mod position;
pub mod util;
pub mod chars;
pub mod name2;
pub mod attribute2;
pub mod event;
