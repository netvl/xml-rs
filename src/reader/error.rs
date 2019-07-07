use std::fmt;
use std::io;
use std::result::Result as StdResult;

use failure::Fail;
use nom::error::{ErrorKind, VerboseError, convert_error};

pub type Result<T> = StdResult<T, Error>;

#[derive(Fail, Debug)]
pub enum Error {
    #[fail(display = "I/O error")]
    Io {
        #[cause]
        cause: io::Error,
    },
    #[fail(display = "Parse error")]
    Parse {
        #[cause]
        cause: ParseError,
    },
}

impl From<io::Error> for Error {
    fn from(cause: io::Error) -> Error {
        Error::Io { cause }
    }
}

impl From<ParseError> for Error {
    fn from(cause: ParseError) -> Error {
        Error::Parse { cause }
    }
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

#[derive(Clone)]
pub struct ParseError {
    message: String,
    debug: String,
}

impl Fail for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.debug)
    }
}
