use std::fmt;
use std::io;
use std::result::Result as StdResult;

use thiserror::Error;
use nom::error::{ErrorKind, VerboseError, convert_error};

pub type Result<T> = StdResult<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("I/O error")]
    Io {
        #[from]
        cause: io::Error,
    },
    
    #[error("Parse error")]
    Parse {
        #[from]
        cause: ParseError,
    },
}

impl<'a> From<(&'a str, VerboseError<&'a str>)> for Error {
    fn from((i, err): (&'a str, VerboseError<&'a str>)) -> Error {
        ParseError { debug: format!("{:#?}", err), message: convert_error(i, err), }.into()
    }
}

impl<'a> From<(&'a str, ErrorKind)> for Error {
    fn from((_, err): (&'a str, ErrorKind)) -> Error {
        ParseError { message: err.description().into(), debug: err.description().into(), }.into()
    }
}

#[derive(Error, Clone)]
#[error("{message}")]
pub struct ParseError {
    message: String,
    debug: String,
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.debug)
    }
}
