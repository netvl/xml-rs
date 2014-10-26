//#![warn(missing_doc)]
#![forbid(non_camel_case_types)]
#![feature(macro_rules, struct_variant, tuple_indexing)]

//! This crate currently provides almost XML 1.0/1.1-compliant pull parser.

extern crate core;

pub use reader::EventReader;

pub mod macros;
pub mod common;
pub mod namespace;
pub mod reader;
pub mod writer;

