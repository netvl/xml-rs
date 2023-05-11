use std::borrow::Cow;
use std::error;
use std::error::Error as _;
use std::fmt;
use std::io;
use std::str;

use crate::common::{Position, TextPosition};
use crate::util;

#[derive(Debug)]
pub enum ErrorKind {
    Syntax(Cow<'static, str>),
    Io(io::Error),
    Utf8(str::Utf8Error),
    UnexpectedEof,
}

/// An XML parsing error.
///
/// Consists of a 2D position in a document and a textual message describing the error.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error {
    pos: TextPosition,
    kind: ErrorKind,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::ErrorKind::{Io, Syntax, UnexpectedEof, Utf8};

        write!(f, "{} ", self.pos)?;
        match &self.kind {
            Io(io_error) => io_error.fmt(f),
            Utf8(reason) => reason.fmt(f),
            Syntax(msg) => f.write_str(msg),
            UnexpectedEof => f.write_str("Unexpected EOF"),
        }
    }
}

impl Position for Error {
    #[inline]
    fn position(&self) -> TextPosition { self.pos }
}

impl Error {
    /// Returns a reference to a message which is contained inside this error.
    #[cold]
    #[doc(hidden)]
    #[allow(deprecated)]
    pub fn msg(&self) -> &str {
        use self::ErrorKind::{Io, Syntax, UnexpectedEof, Utf8};
        match &self.kind {
            Io(io_error) => io_error.description(),
            Utf8(reason) => reason.description(),
            Syntax(msg) => msg.as_ref(),
            UnexpectedEof => "Unexpected EOF",
        }
    }

    #[must_use]
    #[inline]
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}

impl error::Error for Error {
    #[allow(deprecated)]
    #[cold]
    fn description(&self) -> &str { self.msg() }
}

impl<'a, P, M> From<(&'a P, M)> for Error where P: Position, M: Into<Cow<'static, str>> {
    #[cold]
    fn from(orig: (&'a P, M)) -> Self {
        Error {
            pos: orig.0.position(),
            kind: ErrorKind::Syntax(orig.1.into()),
        }
    }
}

impl From<util::CharReadError> for Error {
    #[cold]
    fn from(e: util::CharReadError) -> Self {
        use crate::util::CharReadError::{Io, UnexpectedEof, Utf8};
        Error {
            pos: TextPosition::new(),
            kind: match e {
                UnexpectedEof => ErrorKind::UnexpectedEof,
                Utf8(reason) => ErrorKind::Utf8(reason),
                Io(io_error) => ErrorKind::Io(io_error),
            },
        }
    }
}

impl From<io::Error> for Error {
    #[cold]
    fn from(e: io::Error) -> Self {
        Error {
            pos: TextPosition::new(),
            kind: ErrorKind::Io(e),
        }
    }
}

impl Clone for ErrorKind {
    #[cold]
    fn clone(&self) -> Self {
        use self::ErrorKind::{Io, Syntax, UnexpectedEof, Utf8};
        match self {
            UnexpectedEof => UnexpectedEof,
            Utf8(reason) => Utf8(*reason),
            Io(io_error) => Io(io::Error::new(io_error.kind(), io_error.to_string())),
            Syntax(msg) => Syntax(msg.clone()),
        }
    }
}
impl PartialEq for ErrorKind {
    #[allow(deprecated)]
    fn eq(&self, other: &ErrorKind) -> bool {
        use self::ErrorKind::{Io, Syntax, UnexpectedEof, Utf8};
        match (self, other) {
            (UnexpectedEof, UnexpectedEof) => true,
            (Utf8(left), Utf8(right)) => left == right,
            (Io(left), Io(right)) =>
                left.kind() == right.kind() &&
                left.description() == right.description(),
            (Syntax(left), Syntax(right)) =>
                left == right,

            (_, _) => false,
        }
    }
}
impl Eq for ErrorKind {}
