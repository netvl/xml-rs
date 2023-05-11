//#![warn(missing_doc)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![forbid(non_camel_case_types)]
#![forbid(unsafe_code)]
#![allow(clippy::redundant_closure_for_method_calls)]
#![allow(clippy::module_name_repetitions)]

//! This crate currently provides an almost XML 1.0/1.1-compliant pull parser.

#[cfg(doctest)]
doc_comment::doctest!("../README.md");

pub use crate::reader::EventReader;
pub use crate::reader::ParserConfig;
pub use crate::util::Encoding;
pub use crate::writer::EmitterConfig;
pub use crate::writer::EventWriter;

pub mod attribute;
pub mod common;
pub mod escape;
pub mod macros;
pub mod name;
pub mod namespace;
pub mod reader;
mod util;
pub mod writer;
