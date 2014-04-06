#![crate_id = "xml#0.1"]
#![crate_type = "lib"]
#![warn(missing_doc)]
#![forbid(non_camel_case_types, non_uppercase_statics)]
#![feature(macro_rules, struct_variant)]

//! This crate currently provides almost XML 1.0/1.1-compliant pull parser.


extern crate collections;

pub use reader::EventReader;

pub mod macros;
pub mod common;
pub mod events;
pub mod reader;

