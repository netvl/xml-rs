//#![warn(missing_doc)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![forbid(non_camel_case_types)]

//! This crate currently provides an almost XML 1.0/1.1-compliant pull parser.

pub use crate::reader::EventReader;
pub use crate::reader::ParserConfig;
pub use crate::writer::EventWriter;
pub use crate::writer::EmitterConfig;

pub mod macros;
pub mod name;
pub mod attribute;
pub mod common;
pub mod escape;
pub mod namespace;
pub mod reader;
pub mod writer;
mod util;
