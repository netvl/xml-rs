//! Contains several types used throughout the library.

use std::str;
use std::vec;
use std::hashmap::HashMap;

pub static NS_XMLNS_PREFIX: &'static str = "xmlns";
pub static NS_XMLNS_URI: &'static str    = "http://www.w3.org/2000/xmlns/";
pub static NS_XML_PREFIX: &'static str   = "xml";
pub static NS_XML_URI: &'static str      = "http://www.w3.org/XML/1998/namespace";
pub static NS_EMPTY_URI: &'static str    = "";

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

impl Name {
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
///
/// `None` prefix means no prefix (i.e. default namespace).
#[deriving(Eq, Clone)]
pub struct Namespace(HashMap<Option<~str>, ~str>);

impl Namespace {
    /// Returns an empty namespace.
    pub fn empty() -> Namespace { Namespace(HashMap::with_capacity(2)) }

    /// Puts a mapping into this namespace.
    ///
    /// This method does not override already existing mapping.
    ///
    /// Returns a boolean flag indicating whether the map already contained
    /// the given prefix.
    ///
    /// # Parameters
    /// * `prefix` --- namespace prefix (`None` means default namespace);
    /// * `uri`    --- namespace URI.
    ///
    /// # Return value
    /// `true` if `prefix` has been inserted successfully; `false` if the `prefix`
    /// was already present in the namespace.
    pub fn put(&mut self, prefix: Option<~str>, uri: ~str) -> bool {
        match *self {
            Namespace(ref mut hm) => hm.insert(prefix, uri)
        }
    }

    /// Queries the namespace for the given prefix.
    ///
    /// # Parameters
    /// * `prefix` --- namespace prefix (`None` means default namespace).
    ///
    /// # Return value
    /// Namespace URI corresponding to the given prefix, if it is present.
    pub fn get<'a>(&'a self, prefix: &Option<~str>) -> Option<&'a str> {
        match *self {
            Namespace(ref hm) => hm.find(prefix).map(|s| s.as_slice())
        }
    }
}

/// Namespace stack is a sequence of namespaces. Namespaces are queried from
/// right to left.
#[deriving(Clone, Eq)]
pub struct NamespaceStack(~[Namespace]);

impl NamespaceStack {
    /// Returns an empty namespace stack.
    #[inline]
    pub fn empty() -> NamespaceStack { NamespaceStack(vec::with_capacity(2)) }

    /// Returns a namespace stack with default items in it.
    ///
    /// Default items are the following:
    /// * `xml` → `http://www.w3.org/XML/1998/namespace`;
    /// * `xmlns` → `http://www.w3.org/2000/xmlns/`.
    #[inline]
    pub fn default() -> NamespaceStack {
        let mut nst = NamespaceStack::empty();
        nst.push_empty();
        // xml namespace
        nst.put(Some(NS_XML_PREFIX.to_owned()), NS_XML_URI.to_owned());
        // xmlns namespace
        nst.put(Some(NS_XMLNS_PREFIX.to_owned()), NS_XMLNS_URI.to_owned());
        // empty namespace
        nst.put(None, NS_EMPTY_URI.to_owned());
        nst
    }

    /// Adds an empty namespace to the top of this stack.
    #[inline]
    pub fn push_empty(&mut self) {
        let NamespaceStack(ref mut nst) = *self;
        nst.push(Namespace::empty());
    }

    /// Removes a namespace at the top of the stack.
    ///
    /// Fails if the stack is empty.
    #[inline]
    pub fn pop(&mut self) -> Namespace {
        let NamespaceStack(ref mut nst) = *self;
        nst.pop()
    }

    /// Returns a namespace at the top of the stack, leaving the stack intact.
    ///
    /// Fails if the stack is empty.
    #[inline]
    pub fn peek<'a>(&'a mut self) -> &'a mut Namespace {
        let NamespaceStack(ref mut nst) = *self;
        &mut nst[nst.len()-1]
    }

    /// Puts a mapping into the topmost namespace in this stack.
    ///
    /// This method does not override a mapping in the topmost namespace if it is 
    /// already present, however, it does not depend on other namespaces in the stack.
    ///
    /// Returns a boolean flag indicating whether the topmost namespace 
    /// already contained the given prefix.
    ///
    /// # Parameters
    /// * `prefix` --- namespace prefix (`None` means default namespace);
    /// * `uri`    --- namespace URI.
    ///
    /// # Return value
    /// `true` if `prefix` has been inserted successfully; `false` if the `prefix`
    /// was already present in the namespace.
    #[inline]
    pub fn put(&mut self, prefix: Option<~str>, uri: ~str) -> bool {
        let NamespaceStack(ref mut nst) = *self;
        nst[nst.len()-1].put(prefix, uri)
    }

    /// Performs a search for the given prefix in the whole stack.
    ///
    /// This method walks the stack from top to bottom, querying each namespace
    /// in order for the given prefix. If none of the namespaces contains the prefix,
    /// `None` is returned.
    ///
    /// # Parameters
    /// * `prefix` --- namespace prefix (`None` means default namespace)
    #[inline]
    pub fn get<'a>(&'a self, prefix: &Option<~str>) -> Option<&'a str> {
        let NamespaceStack(ref nst) = *self;
        for ns in nst.rev_iter() {
            match ns.get(prefix) {
                None => {},
                r => return r,
            }
        }
        None
    }

    /// Combines this stack of namespaces into a single namespace.
    ///
    /// Namespaces are combined in left-to-right order, that is, rightmost namespace
    /// elements take priority over leftmost ones.
    pub fn squash(&self) -> Namespace {
        let NamespaceStack(ref nstack) = *self;
        let mut result = HashMap::new();
        for &Namespace(ref ns) in nstack.iter() {
            result.extend(&mut ns.iter().map(|(k, v)| (k.clone(), v.to_owned())));
        }
        Namespace(result)
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
///
/// This function, when finishes sucessfully, always return a qualified
/// name without namespace (`name.namespace == None`). It should be filled later
/// using proper `NamespaceStack`.
///
/// It is supposed that all characters in the argument string are correct
/// as defined by the XML specification. No additional checks except a check
/// for emptiness are done.
pub fn parse_name(name: &str) -> Option<Name> {
    match name.split(':').collect::<~[&str]>() {
        [prefix, local_name] if !prefix.is_empty() && !local_name.is_empty() =>
            Some(Name { prefix: Some(prefix.to_owned()), namespace: None, local_name: local_name.to_owned() }),
        [local_name] if !local_name.is_empty() =>
            Some(Name { prefix: None, namespace: None, local_name: local_name.to_owned() }),
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
pub fn escape_str(s: &str) -> ~str {
    let mut result = str::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '<'  => result.push_str("&lt;"),
            '>'  => result.push_str("&gt;"),
            '"'  => result.push_str("&quot;"),
            '\'' => result.push_str("&apos;"),
            '&'  => result.push_str("&amp;"),
            _    => result.push_char(c)
        }
    }
    result
}
