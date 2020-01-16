pub mod decoding_reader;
pub mod error;
pub mod str_read;

mod config;
mod model;
mod parsing;

pub use config::ReaderConfig;
pub use error::Error;
pub use parsing::Reader;
