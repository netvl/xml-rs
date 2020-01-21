//#![warn(missing_doc)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![forbid(non_camel_case_types)]
#![type_length_limit = "988530600"]

//! This crate currently provides an almost XML 1.0/1.1-compliant pull parser.

#[cfg(test)]
extern crate encoding;
#[cfg(test)]
extern crate quickcheck;

pub use encoding_rs; // Reexport encoding_rs because it is a part of our public API

pub use self::event::Event;
pub use self::reader::{Reader, ReaderConfig};
pub use self::utils::position::Position;
pub use self::writer::{EventBuilder, Writer, WriterConfig};

#[macro_use]
pub mod utils; // At the top to make macros available below

pub mod reader;
pub mod writer;

pub mod attribute;
pub mod event;
pub mod name;
pub mod namespace;
