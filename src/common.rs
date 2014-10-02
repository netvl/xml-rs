//! Contains several types used throughout the library.

use std::fmt;

/// XML parsing error.
///
/// Consists of a row and column reference and a message.
#[deriving(Clone, PartialEq, Eq)]
pub struct Error {
    row: uint,
    col: uint,
    msg: String
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

impl fmt::Show for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}: {}", self.row + 1, self.col + 1, self.msg)
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
    pub fn new<O: HasPosition>(o: &O, msg: String) -> Error {
        Error { row: o.row(), col: o.col(), msg: msg }
    }

    /// Creates a new error using provided position information and a message.
    #[inline]
    pub fn new_full(row: uint, col: uint, msg: String) -> Error {
        Error { row: row, col: col, msg: msg }
    }

    /// Returns a reference to a message which is contained inside this error.
    #[inline]
    pub fn msg<'a>(&'a self) -> &'a str { self.msg.as_slice() }
}

/// XML qualified name.
///
/// Consists of optional prefix, optional namespace and mandatory
/// local name.
#[deriving(Clone, PartialEq, Eq)]
pub struct Name {
    /// An XML namespace prefix.
    ///
    /// This field is always `None` when `namespace` is `None`.
    pub prefix: Option<String>,

    /// An XML namespace identifier.
    pub namespace: Option<String>,

    /// Local (namespace-less) name.
    pub local_name: String
}

impl fmt::Show for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! try_opt(
            ($v:expr) => (
                match $v {
                    Some(e) => try!(e),
                    None => {}
                }
            )
        )

        try_opt!(self.namespace.as_ref().map(|namespace| {
            write!(f, "{{{}}}", namespace)
        }));
        try_opt!(self.prefix.as_ref().map(|prefix| {
            write!(f, "{}:", prefix)
        }));
        write!(f, "{}", self.local_name)
    }
}

impl Name {
    /// Returns a `Name` instance representing plain local name.
    #[inline]
    pub fn new_local(name: &str) -> Name {
        Name {
            local_name: name.to_string(),
            prefix: None,
            namespace: None
        }
    }

    /// Returns a `Name` instance representing qualified name with the
    /// given prefix and namespace URI.
    #[inline]
    pub fn new(name: &str, prefix: &str, namespace: &str) -> Name {
        Name {
            local_name: name.to_string(),
            prefix: Some(prefix.to_string()),
            namespace: Some(namespace.to_string())
        }
    }

    /// Returns a slice with namespace prefix of this name, if it is present.
    pub fn prefix_ref<'a>(&'a self) -> Option<&'a str> {
        match self.prefix {
            None             => None,
            Some(ref prefix) => Some(prefix.as_slice())
        }
    }

    /// Returns a slice with namespace URI of this name, if it is present.
    pub fn namespace_ref<'a>(&'a self) -> Option<&'a str> {
        match self.namespace {
            None                => None,
            Some(ref namespace) => Some(namespace.as_slice())
        }
    }

    /// Returns correct XML representation of this local name and prefix.
    ///
    /// This method is different from autoderived `to_string()` because it does not
    /// include namespace URI in the result.
    pub fn to_str_proper(&self) -> String {
        match self.prefix {
            Some(ref prefix) => format!("{}:{}", prefix, self.local_name),
            None => self.local_name.clone()
        }
    }
}

/// XML element attribute.
///
/// Consistes of a qualified name and a value.
#[deriving(Clone, PartialEq, Eq)]
pub struct Attribute {
    /// Qualified name of the attribute.
    pub name: Name,

    /// Attribute value.
    pub value: String
}

impl fmt::Show for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}=\"{}\"", self.name, escape_str(self.value.as_slice()))
    }
}

impl Attribute {
    /// Returns an `Attribute` instance with the given qualified name and value.
    #[inline]
    pub fn new(name: Name, value: &str) -> Attribute {
        Attribute { name: name, value: value.to_string() }
    }

    /// Returns an `Attribute` instance with plain local name and the given value.
    #[inline]
    pub fn new_local(name: &str, value: &str) -> Attribute {
        Attribute {
            name: Name::new_local(name),
            value: value.to_string()
        }
    }
}

/// XML version enumeration.
#[deriving(Clone, PartialEq, Eq)]
pub enum XmlVersion {
    /// XML version 1.0.
    Version10,

    /// XML version 1.1.
    Version11
}

impl fmt::Show for XmlVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Version10 => write!(f, "1.0"),
            Version11 => write!(f, "1.1")
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
        '\xC0'...'\xD6' | '\xD8'...'\xF6' | '\xF8'...'\u02FF' |
        '\u0370'...'\u037D' | '\u037F'...'\u1FFF' |
        '\u200C'...'\u200D' | '\u2070'...'\u218F' |
        '\u2C00'...'\u2FEF' | '\u3001'...'\uD7FF' |
        '\uF900'...'\uFDCF' | '\uFDF0'...'\uFFFD' |
        '\U00010000'...'\U000EFFFF' => true,
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
        '-' | '.' | '0'...'9' | '\xB7' |
        '\u0300'...'\u03F6' | '\u203F'...'\u2040' => true,
        _ => false
    }
}

/// Parses given string slice into an XML qualified name.
///
/// This function, when finishes sucessfully, always return a qualified
/// name without namespace (`name.namespace == None`). It should be filled later
/// using proper `NamespaceStack`.
///
/// It is supposed that all characters in the argument string are correct
/// as defined by the XML specification. No additional checks except a check
/// for emptiness are done.
pub fn parse_name(name: &str) -> Option<Name> {
    match name.split(':').collect::<Vec<&str>>().as_slice() {
        [prefix, local_name] if !prefix.is_empty() && !local_name.is_empty() =>
            Some(Name { prefix: Some(prefix.to_string()), namespace: None, local_name: local_name.to_string() }),
        [local_name] if !local_name.is_empty() =>
            Some(Name { prefix: None, namespace: None, local_name: local_name.to_string() }),
        _ => None
    }
}

/// Performs escaping of common XML characters.
///
/// This function replaces several important markup characters with their
/// entity equivalents.
///
/// * `<` → `&lt;`
/// * `>` → `&gt;`
/// * `"` → `&quot;`
/// * `'` → `&apos;`
/// * `&` → `&amp;`
///
/// The resulting string is safe to use inside XML attribute values or in
/// PCDATA sections.
pub fn escape_str(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '<'  => result.push_str("&lt;"),
            '>'  => result.push_str("&gt;"),
            '"'  => result.push_str("&quot;"),
            '\'' => result.push_str("&apos;"),
            '&'  => result.push_str("&amp;"),
            _    => result.push(c)
        }
    }
    result
}

/// Contains additional operations on optional values.
pub trait OptionOps<T> {
    /// Executes given action on an optional value, if it is present. Otherwise
    /// it is a no-op.
    fn execute(&self, action: |&T|);
}

impl<T> OptionOps<T> for Option<T> {
    fn execute(&self, action: |&T|) {
        match *self {
            Some(ref value) => action(value),
            None => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Name, Attribute};

    #[test]
    fn attribute_show() {
        let attr = Attribute::new(
            Name::new("attribute", "n", "urn:namespace"),
            "its value with > & \" ' < weird symbols"
        );
        assert_eq!(
            attr.to_string().as_slice(),
            "{urn:namespace}n:attribute=\"its value with &gt; &amp; &quot; &apos; &lt; weird symbols\""
        )
    }
}
