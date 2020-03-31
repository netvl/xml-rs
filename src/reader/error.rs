use std::error::Error as StdError;
use std::fmt;
use std::io;
use std::result::Result as StdResult;

use nom::error::{convert_error, ErrorKind as NomErrorKind, VerboseError};
use derive_more::{Error, Display, From};

use crate::utils::position::TextPosition;
use crate::Position;

pub type Result<T> = StdResult<T, Error>;

#[derive(Debug)]
pub struct Error {
    pub position: TextPosition,
    pub kind: ErrorKind,
}

impl Error {
    pub fn new(source: impl Into<ErrorKind>, position: TextPosition) -> Error {
        Error {
            position,
            kind: source.into(),
        }
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.kind.source()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.position, self.kind)
    }
}

impl Position for Error {
    fn position(&self) -> TextPosition {
        self.position
    }
}

#[derive(Debug, Display, Error, From)]
pub enum ErrorKind {
    #[display(fmt = "I/O error: {}", cause)]
    #[from]
    Io {
        cause: io::Error,
    },

    #[display(fmt = "Parse error: {}", cause)]
    #[from]
    Parse {
        cause: ParseError,
    },
}

impl From<String> for ErrorKind {
    fn from(s: String) -> Self {
        ParseError {
            debug: s.clone(),
            message: s,
        }
        .into()
    }
}

impl<'a> From<&'a str> for ErrorKind {
    fn from(s: &'a str) -> Self {
        ErrorKind::from(s.to_string())
    }
}

impl<'a> From<(&'a str, VerboseError<&'a str>)> for ErrorKind {
    fn from((i, err): (&'a str, VerboseError<&'a str>)) -> Self {
        ParseError {
            debug: format!("{:#?}", err),
            message: convert_error(i, err),
        }
        .into()
    }
}

impl<'a> From<(&'a str, NomErrorKind)> for ErrorKind {
    fn from((_, err): (&'a str, NomErrorKind)) -> Self {
        ParseError {
            message: err.description().into(),
            debug: err.description().into(),
        }
        .into()
    }
}

#[derive(Error, Clone, Display)]
#[display(fmt = "{}", message)]
pub struct ParseError {
    message: String,
    debug: String,
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.debug)
    }
}
