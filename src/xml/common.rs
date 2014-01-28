//! Contains several types used throughout the library.

use std::hashmap::HashMap;

/// XML qualified name.
///
/// Consists of optional prefix, optional namespace and mandatory
/// local name.
#[deriving(Clone, Eq)]
pub struct Name {
    /// An XML namespace prefix.
    ///
    /// This field is always `None` when `namespace` is `None`.
    prefix: Option<~str>,

    /// An XML namespace identifier.
    namespace: Option<~str>,

    /// Local (namespace-less) name.
    local_name: ~str
}

impl ToStr for Name {
    fn to_str(&self) -> ~str {
        use std::str;

        let mut result = str::with_capacity(self.local_name.len());
        if self.namespace.is_some() {
            result.push_str(format!("\\{{}\\}", *self.namespace.get_ref()));
        }
        if self.prefix.is_some() {
            result.push_str(format!("{}:", *self.prefix.get_ref()));
        }
        result.push_str(self.local_name);
        result
    }
}

/// Returned by `parse_name` function when qualified name cannot be parsed.
#[deriving(Clone, Eq)]
pub enum NameParseError {
    /// Returned when the provided string is not a syntactically valid qualified name.
    SyntaxError,

    /// Returned when a prefix in the provided string does not define a valid namespace.
    InvalidPrefix(~str)
}

impl ToStr for NameParseError {
    fn to_str(&self) -> ~str {
        match *self {
            SyntaxError => ~"syntax error",
            InvalidPrefix(ref p) => format!("prefix is invalid: {}", *p)
        }
    }
}

/// XML element attribute.
///
/// Consistes of a qualified name and a value.
#[deriving(Clone, Eq)]
pub struct Attribute {
    /// Qualified name of the attribute.
    name: Name,

    /// Attribute value.
    value: ~str
}

/// XML version enumeration.
#[deriving(Clone, Eq)]
pub enum XmlVersion {
    /// XML version 1.0.
    Version10,

    /// XML version 1.1.
    Version11
}

impl ToStr for XmlVersion {
    fn to_str(&self) -> ~str {
        match *self {
            Version10 => ~"1.0",
            Version11 => ~"1.1"
        }
    }
}

/// XML parsing error.
///
/// Consists of a row and column reference and a message.
#[deriving(Clone, Eq)]
pub struct Error {
    priv row: uint,
    priv col: uint,
    priv msg: ~str
}

/// Represents a thing which has a position inside some textual document.
///
/// This trait is implemented by parsers, lexers and errors. It is used primarily to create
/// error objects.
pub trait HasPosition {
    /// Returns a line number inside the document.
    fn row(&self) -> uint;

    /// Returns a column number inside the document.
    fn col(&self) -> uint;
}

impl ToStr for Error {
    #[inline]
    fn to_str(&self) -> ~str {
        format!("{}:{}: {}", self.row + 1, self.col + 1, self.msg)
    }
}

impl HasPosition for Error {
    #[inline]
    fn row(&self) -> uint { self.row }

    #[inline]
    fn col(&self) -> uint { self.col }
}

impl Error {
    /// Creates a new error using position information from the provided
    /// `HasPosition` object and a message.
    #[inline]
    pub fn new<O: HasPosition>(o: &O, msg: ~str) -> Error {
        Error { row: o.row(), col: o.col(), msg: msg }
    }

    /// Creates a new error using provided position information and a message.
    #[inline]
    pub fn new_full(row: uint, col: uint, msg: ~str) -> Error {
        Error { row: row, col: col, msg: msg }
    }

    /// Returns a reference to a message which is contained inside this error.
    #[inline]
    pub fn msg<'a>(&'a self) -> &'a str { self.msg.as_slice() }
}

/// Namespace is a map from prefixes to namespace URIs.
type Namespace = HashMap<~str, ~str>;

/// Namespace stack is a sequence of namespaces. Namespaces are queried from
/// right to left.
type NamespaceStack = ~[Namespace];

/// Combines a stack of namespaces into single namespace.
///
/// Namespaces are combined in left-to-right manner, that is, rightmost namespace
/// elements take priority over leftmost ones.
///
/// # Parameters
/// * `st` --- a slice of namespaces (can be used directly with `NamespaceStack`)
pub fn squash_ns_stack(st: &[Namespace]) -> Namespace {
    let mut result = HashMap::new();
    for ns in st.iter() {
        result.extend(&mut ns.iter().map(|(k, v)| (k.to_owned(), v.to_owned())));
    }
    result
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
        ':' | 'A'..'Z' | '_' | 'a'..'z' |
        '\xC0'..'\xD6' | '\xD8'..'\xF6' | '\xF8'..'\u02FF' |
        '\u0370'..'\u037D' | '\u037F'..'\u1FFF' |
        '\u200C'..'\u200D' | '\u2070'..'\u218F' |
        '\u2C00'..'\u2FEF' | '\u3001'..'\uD7FF' |
        '\uF900'..'\uFDCF' | '\uFDF0'..'\uFFFD' |
        '\U00010000'..'\U000EFFFF' => true,
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
        '-' | '.' | '0'..'9' | '\xB7' | 
        '\u0300'..'\u03F6' | '\u203F'..'\u2040' => true,
        _ => false
    }
}

/// Parses given string slice into an XML qualified name.
/// TODO: add namespace map as a parameter
pub fn parse_name(name: &str) -> Result<Name, NameParseError> {
    // TODO: actual implementation
    Ok(Name {
        prefix: None,
        namespace: None,
        local_name: name.to_owned()
    })
}
