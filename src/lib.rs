//#![warn(missing_doc)]
#![allow(unstable)]
#![forbid(non_camel_case_types)]
#![feature(slicing_syntax, old_impl_check)]

//! This crate currently provides almost XML 1.0/1.1-compliant pull parser.

extern crate core;

pub use reader::EventReader;

pub mod macros;
pub mod name;
pub mod attribute;
pub mod common;
pub mod escape;
pub mod namespace;
pub mod reader;
pub mod writer;
pub mod util;
