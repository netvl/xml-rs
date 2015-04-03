use std::borrow::Cow;
use std::iter::Rev;
use std::collections::hash_map::{HashMap, Entry};
use std::collections::hash_map::Iter as Entries;
use std::collections::HashSet;
use std::slice::Iter;

use util::{OptionBorrowExt, IteratorClonedPairwiseExt};

pub const NS_XMLNS_PREFIX: &'static str = "xmlns";
pub const NS_XMLNS_URI: &'static str    = "http://www.w3.org/2000/xmlns/";
pub const NS_XML_PREFIX: &'static str   = "xml";
pub const NS_XML_URI: &'static str      = "http://www.w3.org/XML/1998/namespace";
pub const NS_EMPTY_URI: &'static str    = "";

pub type UriMapping<'a> = (Option<&'a str>, &'a str);

/// Denotes something which contains namespace URI mappings.
///
/// A URI mapping is a pair of type `(Option<&str>, &str)`, where the first item
/// is a namespace prefix (`None` meaning default prefix) and the second item
/// is a URI mapped to this prefix.
pub trait NamespaceIterable<'a> {
    type Iter: Iterator<Item=UriMapping<'a>>;
    fn uri_mappings(&'a self) -> Self::Iter;
}

/// Namespace is a map from prefixes to namespace URIs.
///
/// `None` prefix means no prefix (i.e. default namespace).
#[derive(PartialEq, Clone)]
pub struct Namespace(pub HashMap<Option<String>, String>);

impl Namespace {
    /// Returns an empty namespace.
    #[inline]
    pub fn empty() -> Namespace { Namespace(HashMap::with_capacity(3)) }

    /// Checks whether this namespace is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Checks whether this namespace is essentially empty, that is, it does not contain
    /// anything but the default mappings.
    pub fn is_essentially_empty(&self) -> bool {
        // a shortcut for a namespace which is definitely not empty
        if self.0.len() > 3 { return false; }

        self.0.iter().all(|(k, v)| match (k.borrow_internals(), &v[..]) {
            (None,                  NS_EMPTY_URI) => true,
            (Some(NS_XMLNS_PREFIX), NS_XMLNS_URI) => true,
            (Some(NS_XML_PREFIX),   NS_XML_URI)   => true,
            _ => false
        })
    }

    /// Puts a mapping into this namespace.
    ///
    /// This method does not override an already existing mapping.
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
    pub fn put<'s1, 's2, S1, S2>(&mut self, prefix: Option<S1>, uri: S2) -> bool
            where S1: Into<Cow<'s1, str>>, S2: Into<Cow<'s2, str>> {
        match self.0.entry(prefix.map(|v| v.into().into_owned())) {
            Entry::Occupied(_) => false,
            Entry::Vacant(ve) => {
                ve.insert(uri.into().into_owned());
                true
            }
        }
    }

    /// Puts a mapping into this namespace forcefully.
    ///
    /// This method, as opposed to `put()`, does replace an already existing mapping.
    ///
    /// Returns previous URI which was assigned to the given prefix, if it is present.
    ///
    /// # Parameters
    /// * `prefix` --- namespace prefix (`None` means default namespace);
    /// * `uri`    --- namespace URI.
    ///
    /// # Return value
    /// `Some(uri)` with `uri` being a previous URI assigned to the `prefix`, or
    /// `None` if such prefix was not present in the namespace before.
    pub fn force_put<'s1, 's2, S1, S2>(&mut self, prefix: Option<S1>, uri: S2) -> Option<String>
            where S1: Into<Cow<'s1, str>>, S2: Into<Cow<'s2, str>> {
        self.0.insert(prefix.map(|v| v.into().into_owned()), uri.into().into_owned())
    }

    /// Queries the namespace for the given prefix.
    ///
    /// # Parameters
    /// * `prefix` --- namespace prefix (`None` means default namespace).
    ///
    /// # Return value
    /// Namespace URI corresponding to the given prefix, if it is present.
    pub fn get<'a>(&'a self, prefix: &Option<String>) -> Option<&'a str> {
        self.0.get(prefix).map(|s| &**s)
    }
}

/// An iterator over mappings from prefixes to URIs in a namespace.
///
/// This is a separate structure because `map()` operation on iterators uses an unboxed
/// closure type which can't be named.
pub struct NamespaceMappings<'a> {
    entries: Entries<'a, Option<String>, String>
}

impl<'a> Iterator for NamespaceMappings<'a> {
    type Item = (Option<&'a str>, &'a str);

    fn next(&mut self) -> Option<(Option<&'a str>, &'a str)> {
        self.entries.next().map(|(prefix, uri)| {
            (prefix.as_ref().map(|p| &p[..]), &uri[..])
        })
    }
}

impl<'a> NamespaceIterable<'a> for Namespace {
    type Iter = NamespaceMappings<'a>;

    fn uri_mappings(&'a self) -> NamespaceMappings<'a> {
        NamespaceMappings { entries: self.0.iter() }
    }
}

/// Namespace stack is a sequence of namespaces.
///
/// Namespace stack is used to represent cumulative namespace consisting of
/// combined namespaces from nested elements.
#[derive(Clone, PartialEq)]
pub struct NamespaceStack(pub Vec<Namespace>);

impl NamespaceStack {
    /// Returns an empty namespace stack.
    #[inline]
    pub fn empty() -> NamespaceStack { NamespaceStack(Vec::with_capacity(2)) }

    /// Returns a namespace stack with default items in it.
    ///
    /// Default items are the following:
    ///
    /// * `xml` → `http://www.w3.org/XML/1998/namespace`;
    /// * `xmlns` → `http://www.w3.org/2000/xmlns/`.
    #[inline]
    pub fn default() -> NamespaceStack {
        let mut nst = NamespaceStack::empty();
        nst.push_empty();
        // xml namespace
        nst.put(Some(NS_XML_PREFIX.to_string()), NS_XML_URI.to_string());
        // xmlns namespace
        nst.put(Some(NS_XMLNS_PREFIX.to_string()), NS_XMLNS_URI.to_string());
        // empty namespace
        nst.put(None, NS_EMPTY_URI.to_string());
        nst
    }

    /// Adds an empty namespace to the top of this stack.
    #[inline]
    pub fn push_empty(&mut self) {
        self.0.push(Namespace::empty());
    }

    /// Removes a namespace at the top of the stack.
    ///
    /// Fails if the stack is empty.
    #[inline]
    pub fn pop(&mut self) -> Namespace {
        self.0.pop().unwrap()
    }

    /// Returns a namespace at the top of the stack, leaving the stack intact.
    ///
    /// Fails if the stack is empty.
    #[inline]
    pub fn peek<'a>(&'a mut self) -> &'a mut Namespace {
        self.0.last_mut().unwrap()
    }

    /// Puts a mapping into the topmost namespace in this stack.
    ///
    /// This method does not override a mapping in the topmost namespace if it is
    /// already present, however, it does not depend on other namespaces in the stack,
    /// so it is possible to put a mapping which is present in lower namespaces.
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
    pub fn put(&mut self, prefix: Option<String>, uri: String) -> bool {
        self.0.last_mut().unwrap().put(prefix, uri)
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
    pub fn get<'a>(&'a self, prefix: &Option<String>) -> Option<&'a str> {
        for ns in self.0.iter().rev() {
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
        let mut result = HashMap::new();
        for ns in self.0.iter() {
            result.extend(ns.0.iter().cloned_pairwise());
        }
        Namespace(result)
    }
}

/// An iterator over mappings from prefixes to URIs in a namespace stack.
pub struct NamespaceStackMappings<'a> {
    namespaces: Rev<Iter<'a, Namespace>>,
    current_namespace: Option<NamespaceMappings<'a>>,
    used_keys: HashSet<Option<&'a str>>
}

impl<'a> NamespaceStackMappings<'a> {
    fn to_next_namespace(&mut self) -> bool {
        self.current_namespace = self.namespaces.next().map(|ns| ns.uri_mappings());
        self.current_namespace.is_some()
    }
}

impl<'a> Iterator for NamespaceStackMappings<'a> {
    type Item = (Option<&'a str>, &'a str);

    fn next(&mut self) -> Option<(Option<&'a str>, &'a str)> {
        // If there is no current namespace and no next namespace, we're finished
        if self.current_namespace.is_none() && !self.to_next_namespace() {
            return None;
        }
        let next_item = self.current_namespace.as_mut().unwrap().next();

        match next_item {
            // There is an element in the current namespace
            Some((k, v)) => if self.used_keys.contains(&k) {
                // If the current key is used, go to the next one
                self.next()
            } else {
                // Otherwise insert the current key to the set of used keys and
                // return the mapping
                self.used_keys.insert(k);
                Some((k, v))
            },
            // Current namespace is exhausted
            None => if self.to_next_namespace() {
                // If there is next namespace, continue from it
                self.next()
            } else {
                // No next namespace, exiting
                None
            }
        }
    }
}

impl<'a> NamespaceIterable<'a> for NamespaceStack {
    type Iter = NamespaceStackMappings<'a>;

    fn uri_mappings(&'a self) -> NamespaceStackMappings<'a> {
        NamespaceStackMappings {
            namespaces: self.0.iter().rev(),
            current_namespace: None,
            used_keys: HashSet::new()
        }
    }
}
