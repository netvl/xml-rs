

mod config;
mod error;
mod parser;
#[cfg(feature = "encodings")]
pub mod encodings;

pub use self::parser::Parser;
