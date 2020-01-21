pub mod error;

mod config;
mod data;
mod model;
mod reading;

pub use config::ReaderConfig;
pub use error::{Error, Result};
pub use reading::Reader;
