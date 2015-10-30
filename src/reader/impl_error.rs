
use std::error;
use std::fmt;

use super::util;
use common::{Position, TextPosition};
use super::{Error, ErrorKind};

impl Error {
    pub fn new( kind: ErrorKind ) -> Self {
        Error {
            pos: TextPosition::new(),
            kind: kind,
        }
    }
    pub fn new_with_pos( pos: TextPosition, kind: ErrorKind ) -> Self {
        Error {
            pos: pos,
            kind: kind,
        }
    }
    pub fn kind(&self) -> &ErrorKind { &self.kind }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
       write!(f, "{} {}", self.position(), self.msg())
    }
}

impl Position for Error {
    #[inline]
    fn position(&self) -> TextPosition { self.pos }
}

impl Error {
    /// Returns a reference to a message which is contained inside this error.
    #[inline]
    pub fn msg(&self) -> &str {
        use super::ErrorKind::*;
        use std::error::Error;
        match self.kind {
            UnexpectedEof => "Unexpected end of stream",
            Utf8( ref e ) => e.description(),
            Syntax( ref s ) => s.as_ref(),
            Io( ref e ) => e.description(),
        }
    }
}

impl error::Error for Error {
    #[inline]
    fn description(&self) -> &str { self.msg() }
    fn cause( &self ) -> Option<&error::Error> { None }
}



impl Clone for ErrorKind {
    fn clone( &self ) -> Self {
        use super::ErrorKind::*;
        match *self {
            UnexpectedEof => UnexpectedEof,
            Utf8( ref e ) => Utf8( e.clone() ),
            Syntax( ref e ) => Syntax( e.clone() ),
            Io( ref e ) => Io( util::clone_io_error( e ) ),
        }
    }
}
impl PartialEq for ErrorKind {
    fn eq( &self, other: &ErrorKind ) -> bool {
        use super::ErrorKind::*;
        match ( self, other ) {
            ( &UnexpectedEof, &UnexpectedEof ) => true,
            ( &Utf8( ref left ), &Utf8( ref right ) ) => left == right,
            ( &Syntax( ref left ), &Syntax( ref right ) ) => left == right,
            ( &Io( ref left ), &Io( ref right ) ) => left.kind() == right.kind(),
            ( _, _ ) => false,
        }
    }
}
impl Eq for ErrorKind {}
