//#![warn(missing_doc)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![forbid(non_camel_case_types)]
#![forbid(unsafe_code)]
#![type_length_limit = "988530600"]

//! This crate currently provides an almost XML 1.0/1.1-compliant pull parser.

#[cfg(test)]
extern crate encoding;
#[cfg(test)]
extern crate quickcheck;

#[cfg(doctest)]
#[macro_use]
extern crate doc_comment;

#[cfg(doctest)]
doctest!("../Readme.md");

pub use encoding_rs;

pub use reader::{Reader, ReaderConfig};
pub use writer::{Writer, WriterConfig};

pub mod chars;
pub mod event;
#[macro_use]
pub mod macros;
pub mod namespace;
pub mod position;
pub mod writer;

pub mod attribute;
pub mod name;

pub mod reader;
