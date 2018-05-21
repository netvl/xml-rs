use std::io;
use std::result::Result as StdResult;
use std::fmt;

use failure::Fail;

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
        Error::Io { cause, }
    }
}

impl From<ParseError> for Error {
    fn from(cause: ParseError) -> Error {
        Error::Parse { cause, }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedEof { expected: Option<Vec<String>> },
    UnexpectedToken { actual: String, expected: Vec<String>, },
    UnexpectedDeclaration,
    UnexpectedDoctype,
    UnclosedAttributeValue,
    InvalidAttributeName { name: String, },
    InvalidDeclaration(InvalidDeclarationReason),
}

#[derive(Debug, Clone)]
pub enum InvalidDeclarationReason {
    MissingVersion,
    InvalidVersion { version: String, },
    InvalidStandalone { standalone: String, },
    UnexpectedContent { content: String, }
}

impl fmt::Display for InvalidDeclarationReason {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InvalidDeclarationReason::MissingVersion =>
                write!(f, "missing version"),
            InvalidDeclarationReason::InvalidVersion { version, } =>
                write!(f, "version is not valid: {}", version),
            InvalidDeclarationReason::InvalidStandalone { standalone, } =>
                write!(f, "standalone flag is not valid: {}", standalone),
            InvalidDeclarationReason::UnexpectedContent { content, } =>
                write!(f, "unexpected content: {}", content),
        }
    }
}

impl InvalidDeclarationReason {
    pub fn unexpected_content(content: impl Into<String>) -> InvalidDeclarationReason {
        InvalidDeclarationReason::UnexpectedContent { content: content.into(), }
    }

    pub fn invalid_standalone(standalone: impl Into<String>) -> InvalidDeclarationReason {
        InvalidDeclarationReason::InvalidStandalone { standalone: standalone.into(), }
    }

    pub fn invalid_version(version: impl Into<String>) -> InvalidDeclarationReason {
        InvalidDeclarationReason::InvalidVersion { version: version.into(), }
    }
}

impl ParseError {
    pub fn unexpected_eof_no_expected() -> ParseError {
        ParseError::UnexpectedEof { expected: None, }
    }

    pub fn unexpected_eof<I, S>(expected: Option<I>) -> ParseError
        where I: IntoIterator<Item=S>, S: fmt::Display,
    {
        ParseError::UnexpectedEof { expected: expected.map(display_iter), }
    }

    pub fn unexpected_token<I, S>(actual: impl Into<String>, expected: I) -> ParseError
        where I: IntoIterator<Item=S>, S: fmt::Display
    {
        ParseError::UnexpectedToken { actual: actual.into(), expected: display_iter(expected), }
    }

    pub fn invalid_name(name: impl Into<String>) -> ParseError {
        ParseError::InvalidAttributeName { name: name.into(), }
    }
}

impl Fail for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedEof { expected: None, } => write!(f, "Unexpected end of stream"),
            ParseError::UnexpectedEof { expected: Some(expected), } =>
                write!(f, "Unexpected end of stream, expected: {}", expected.join(", or ")),
            ParseError::UnexpectedToken { actual, expected, } =>
                write!(f, "Unexpected token: {}, expected: {}", actual, expected.join(", or ")),
            ParseError::UnexpectedDeclaration =>
                write!(f, "Unexpected XML declaration"),
            ParseError::UnexpectedDoctype =>
                write!(f, "Unexpected document type definition"),
            ParseError::UnclosedAttributeValue =>
                write!(f, "Unclosed attribute value"),
            ParseError::InvalidAttributeName { name, } =>
                write!(f, "Attribute name is not valid"),
            ParseError::InvalidDeclaration(reason) =>
                write!(f, "XML document declaration is not valid: {}", reason)
        }
    }
}

fn display_iter<I, S>(iter: I) -> Vec<String>
    where I: IntoIterator<Item=S>, S: fmt::Display,
{
    iter.into_iter().map(|s| s.to_string()).collect()
}
