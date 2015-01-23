//! Contains several types used throughout the library.

use std::fmt;
use std::error;

/// Represents a thing which has a position inside some textual document.
///
/// This trait is implemented by parsers, lexers and errors. It is used primarily to create
/// error objects.
pub trait HasPosition {
    /// Returns a line number inside the document.
    fn row(&self) -> usize;

    /// Returns a column number inside the document.
    fn col(&self) -> usize;
}

/// XML parsing error.
///
/// Consists of a row and column reference and a message.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error {
    row: usize,
    col: usize,
    msg: String
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}: {}", self.row + 1, self.col + 1, self.msg)
    }
}

impl HasPosition for Error {
    #[inline]
    fn row(&self) -> usize { self.row }

    #[inline]
    fn col(&self) -> usize { self.col }
}

impl Error {
    /// Creates a new error using position information from the provided
    /// `HasPosition` object and a message.
    #[inline]
    pub fn new<O: HasPosition>(o: &O, msg: String) -> Error {
        Error { row: o.row(), col: o.col(), msg: msg }
    }

    /// Creates a new error using provided position information and a message.
    #[inline]
    pub fn new_full(row: usize, col: usize, msg: String) -> Error {
        Error { row: row, col: col, msg: msg }
    }

    /// Returns a reference to a message which is contained inside this error.
    #[inline]
    pub fn msg<'a>(&'a self) -> &'a str { self.msg.as_slice() }
}

impl error::Error for Error {
    #[inline]
    fn description(&self) -> &str { &*self.msg }
}

/// XML version enumeration.
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum XmlVersion {
    /// XML version 1.0.
    Version10,

    /// XML version 1.1.
    Version11
}

impl fmt::Display for XmlVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            XmlVersion::Version10 => write!(f, "1.0"),
            XmlVersion::Version11 => write!(f, "1.1")
        }
    }
}


/// Checks whether the given character is a white space character (`S`)
/// as is defined by XML 1.1 specification, [section 2.3][1].
///
/// [1]: http://www.w3.org/TR/2006/REC-xml11-20060816/#sec-common-syn
pub fn is_whitespace_char(c: char) -> bool {
    match c {
        '\x20' | '\x09' | '\x0d' | '\x0a' => true,
        _ => false
    }
}

/// Checks whether the given character is a name start character (`NameStartChar`)
/// as is defined by XML 1.1 specification, [section 2.3][1].
///
/// [1]: http://www.w3.org/TR/2006/REC-xml11-20060816/#sec-common-syn
pub fn is_name_start_char(c: char) -> bool {
    match c {
        ':' | 'A'...'Z' | '_' | 'a'...'z' |
        '\u{C0}'...'\u{D6}' | '\u{D8}'...'\u{F6}' | '\u{F8}'...'\u{2FF}' |
        '\u{370}'...'\u{37D}' | '\u{37F}'...'\u{1FFF}' |
        '\u{200C}'...'\u{200D}' | '\u{2070}'...'\u{218F}' |
        '\u{2C00}'...'\u{2FEF}' | '\u{3001}'...'\u{D7FF}' |
        '\u{F900}'...'\u{FDCF}' | '\u{FDF0}'...'\u{FFFD}' |
        '\u{10000}'...'\u{EFFFF}' => true,
        _ => false
    }
}

/// Checks whether the given character is a name character (`NameChar`)
/// as is defined by XML 1.1 specification, [section 2.3][1].
///
/// [1]: http://www.w3.org/TR/2006/REC-xml11-20060816/#sec-common-syn
pub fn is_name_char(c: char) -> bool {
    match c {
        _ if is_name_start_char(c) => true,
        '-' | '.' | '0'...'9' | '\u{B7}' |
        '\u{300}'...'\u{3F6}' | '\u{203F}'...'\u{2040}' => true,
        _ => false
    }
}

