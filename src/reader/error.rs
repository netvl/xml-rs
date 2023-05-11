use std::borrow::Cow;
use std::error;
use std::error::Error as _;
use std::fmt;
use std::io;
use std::str;
use std::error;

use crate::common::{Position, TextPosition};
use crate::util;

#[derive(Debug)]
pub enum ErrorKind {
    Syntax(SyntaxError),
    Io(io::Error),
    Utf8(str::Utf8Error),
    UnexpectedEof,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ErrorKind::*;
        match self {
            Syntax(err) => write!(f, "Syntax error: {}", err),
            Io(err) => write!(f, "IO error: {}", err),
            Utf8(err) => write!(f, "Utf8 encoding error: {}", err),
            UnexpectedEof => write!(f, "Unexpected EOF"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SyntaxError {
    UnexpectedEof,
    NoRootElement,
    UnbalancedRootElement,
    InvalidQualifiedName(String),
    UnexpectedQualifiedName(Token),
    UnexpectedOpeningTag,
    MissingNamespace(OwnedName),
    UnboundAttribute(OwnedName),
    UnboundPrefix(OwnedName),
    UnexpectedClosingTag(OwnedName, OwnedName),
    UnexpectedTokenBefore(&'static str, char),
    UnexpectedTokenOutsideRoot(Token),
    UnexpectedToken(Token),
    UnexpectedTokenInEntity(Token),
    UnexpectedTokenInClosingTag(Token),
    UnexpectedTokenInOpeningTag(Token),
    UnexpectedName(OwnedName),
    ProcessingInstructionWithoutName,
    /// Found <?xml-like PI not at the beginning of a document,
    /// which is an error, see section 2.6 of XML 1.1 spec
    InvalidXmlProcessingInstruction(String),
    InvalidProcessingInstruction(String),
    UnexpectedProcessingInstruction(String, Token),
    InvalidNamePrefix(Option<String>),
    RedefinedAttribute(OwnedName),
    CannotUndefinePrefix(String),
    CannotRedefineXmlnsPrefix,
    CannotRedefineXmlPrefix,
    UnexpectedTokenInsideXml(Token),
    UnexpectedNameInsideXml(OwnedName),
    UnexpectedXmlVersion(Option<XmlVersion>),
    InvalidStandaloneDeclaration(String),
    EmptyEntity,
    NullCharacterEntity,
    InvalidHexCharacterEntity(String),
    InvalidDecCharacterEntity(String),
    UnexpectedEntity(String),
    InvalidDefaultNamespace(String),
    /// Double dash ("--") is illegal inside a comment
    DoubleDashInComment,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::SyntaxError::*;
        match self {
            UnexpectedEof => write!(f, "Unexpected end of stream"),
            NoRootElement => write!(f, "Unexpected end of stream: no root element found"),
            UnbalancedRootElement => write!(f, "Unexpected end of stream: still inside the root element"),
            InvalidQualifiedName(e) => write!(f, "Qualified name is invalid: {}", e),
            UnexpectedQualifiedName(e) => write!(f, "Unexpected token inside qualified name: {}", e),
            UnexpectedOpeningTag => write!(f, "Unexpected token inside attribute value: <"),
            MissingNamespace(name) => write!(f, "Element {} prefix is unbound", name),
            UnboundAttribute(name) => write!(f, "Attribute {} prefix is unbound", name),
            UnboundPrefix(name) => write!(f, "Element {} prefix is unbound", name),
            UnexpectedClosingTag(expected_name, got_name) => write!(f, "Unexpected closing tag: {}, expected {}", expected_name, got_name),
            UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
            UnexpectedTokenBefore(before, c) => write!(f, "Unexpected token '{}' before '{}'", before, c),
            UnexpectedTokenOutsideRoot(token) => write!(f, "Unexpected characters outside the root element: {}", token),
            UnexpectedTokenInOpeningTag(token) => write!(f, "Unexpected token inside opening tag: {}", token),
            UnexpectedTokenInClosingTag(token) => write!(f, "Unexpected token inside closing tag: {}", token),
            UnexpectedTokenInEntity(token) => write!(f, "Unexpected token inside entity: {}", token),
            UnexpectedName(name) => write!(f, "Unexpected name: {}", name),
            ProcessingInstructionWithoutName => write!(f, "Encountered processing instruction without name"),
            InvalidXmlProcessingInstruction(name) => write!(f,
                "Invalid processing instruction: <?{} - \"<?xml\"-like PI is \
                 only valid at the beginning of the document", name),
            InvalidProcessingInstruction(name) => write!(f, "Invalid processing instruction: <?{}", name),
            UnexpectedProcessingInstruction(buf, token) => write!(f, "Unexpected token inside processing instruction: <?{}{}", buf, token),
            InvalidNamePrefix(Some(prefix)) => write!(f, "'{}' cannot be an element name prefix", prefix),
            InvalidNamePrefix(None) => write!(f, "Empty element name prefix"),
            RedefinedAttribute(name) => write!(f, "Attribute '{}' is redefined", name),
            CannotUndefinePrefix(ln) => write!(f, "Cannot undefine prefix '{}'", ln),
            CannotRedefineXmlnsPrefix => write!(f, "Cannot redefine XMLNS prefix '{}'", NS_XMLNS_PREFIX),
            CannotRedefineXmlPrefix => write!(f, "Prefix '{}' cannot be rebound to another value", NS_XML_PREFIX),
            UnexpectedTokenInsideXml(token) => write!(f, "Unexpected token inside XML declaration: {}", token),
            UnexpectedNameInsideXml(name) => write!(f, "Unexpected name inside XML declaration: {}", name),
            UnexpectedXmlVersion(Some(version)) => write!(f, "Invalid XML version: {}", version),
            UnexpectedXmlVersion(None) => write!(f, "No XML version specified"),
            InvalidStandaloneDeclaration(value) => write!(f, "Invalid standalone declaration value: {}", value),
            EmptyEntity => write!(f, "Encountered empty entity"),
            NullCharacterEntity => write!(f, "Null character entity is not allowed"),
            InvalidHexCharacterEntity(name) => write!(f, "Invalid hexadecimal character number in an entity: {}", name),
            InvalidDecCharacterEntity(name) => write!(f, "Invalid decimal character number in an entity: {}", name),
            UnexpectedEntity(name) => write!(f, "Unexpected entity: {}", name),
            InvalidDefaultNamespace(name) => write!(f,  "Namespace '{}' cannot be default", name),
            DoubleDashInComment => write!(f, "Unexpected double dash inside a comment: \"--\""),
        }
    }
}

/// An XML parsing error.
///
/// Consists of a 2D position in a document and a textual message describing the error.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error {
    pub pos: TextPosition,
    pub kind: ErrorKind,
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

impl error::Error for Error { }

impl Error {
    #[inline]
    fn kind(&self) -> &ErrorKind { &self.kind }
}

impl From<io::Error> for Error {
    #[cold]
    fn from(e: io::Error) -> Self {
        Error {
            pos: TextPosition::new(),
            kind: ErrorKind::Io(e)
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
