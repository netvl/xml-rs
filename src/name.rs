use std::fmt;
use std::str::FromStr;
use std::borrow::{Cow, ToOwned};

use util::OptionBorrowExt;

/// Represents a qualified XML name.
///
/// A qualified name always consists at least of a local name. It can optionally contain
/// a prefix; if it contains a prefix, it must also contain a namespace URI, but this is not
/// enforced statically; see below. The name can contain a namespace without a prefix; in
/// that case a default, empty prefix is assumed.
///
/// # Prefixes and URIs
///
/// A qualified name with a prefix must always contain a proper namespace URI --- names with
/// a prefix but without a namespace associated with that prefix are meaningless. However,
/// it is impossible to obtain proper namespace URY by a prefix without a context, and such
/// context is only available when parsing a document (or it can be constructed manually
/// when writing a document). Tying a name to a context statically seems impractical. This
/// may change in future, though.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Name<'a> {
    /// A local name, e.g. `string` in `xsi:string`.
    pub local_name: &'a str,

    /// A namespace URI, e.g. `http://www.w3.org/2000/xmlns/`.
    pub namespace: Option<&'a str>,

    /// A name prefix, e.g. `xsi` in `xsi:string`.
    pub prefix: Option<&'a str>
}

impl<'a> fmt::Display for Name<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(namespace) = self.namespace {
            try! { write!(f, "{{{}}}", namespace) }
        }

        if let Some(prefix) = self.prefix {
            try! { write!(f, "{}:", prefix) }
        }

        write!(f, "{}", self.local_name)
    }
}

impl<'a> Name<'a> {
    /// Returns an owned variant of the qualified name.
    pub fn to_owned(&self) -> OwnedName {
        OwnedName {
            local_name: self.local_name.to_owned(),
            namespace: self.namespace.map(|s| s.to_owned()),
            prefix: self.prefix.map(|s| s.to_owned())
        }
    }

    /// Returns a new `Name` instance representing plain local name.
    #[inline]
    pub fn local(local_name: &str) -> Name {
        Name {
            local_name: local_name,
            prefix: None,
            namespace: None
        }
    }

    /// Returns a new `Name` instance representing a qualified name with or without a prefix and
    /// with a namespace URI.
    #[inline]
    pub fn qualified(local_name: &'a str, namespace: &'a str, prefix: Option<&'a str>) -> Name<'a> {
        Name {
            local_name: local_name,
            namespace: Some(namespace),
            prefix: prefix,
        }
    }

    /// Returns correct XML representation of this local name and prefix.
    ///
    /// This method is different from the autoimplemented `to_string()` because it does not
    /// include namespace URI in the result.
    pub fn to_repr(&self) -> String {
        match self.prefix {
            Some(prefix) => format!("{:?}:{:?}", prefix, self.local_name),
            None => self.local_name.to_owned()
        }
    }
}

/// An owned variant of `Name`.
///
/// Everything about `Name` applies to this structure as well.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct OwnedName {
    /// A local name, e.g. `string` in `xsi:string`.
    pub local_name: String,

    /// A namespace URI, e.g. `http://www.w3.org/2000/xmlns/`.
    pub namespace: Option<String>,

    /// A name prefix, e.g. `xsi` in `xsi:string`.
    pub prefix: Option<String>,
}

impl fmt::Display for OwnedName {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.borrow(), f)
    }
}

impl OwnedName {
    /// Constructs a borrowed `Name` based on this owned name.
    pub fn borrow(&self) -> Name {
        Name {
            local_name: &*self.local_name,
            namespace: self.namespace.as_ref().map(|s| &**s),
            prefix: self.prefix.as_ref().map(|s| &**s),
        }
    }

    /// Returns a new `OwnedName` instance representing a plain local name.
    #[inline]
    pub fn local<'s, S: Into<Cow<'s, str>>>(local_name: S) -> OwnedName {
        OwnedName {
            local_name: local_name.into().into_owned(),
            namespace: None,
            prefix: None,
        }
    }

    /// Returns a new `OwnedName` instance representing a qualified name with or without
    /// a prefix and with a namespace URI.
    #[inline]
    pub fn qualified<'s1, 's2, 's3, S1, S2, S3>(local_name: S1, namespace: S2,
                                                prefix: Option<S3>) -> OwnedName
            where S1: Into<Cow<'s1, str>>,
                  S2: Into<Cow<'s2, str>>,
                  S3: Into<Cow<'s3, str>> {
        OwnedName {
            local_name: local_name.into().into_owned(),
            namespace: Some(namespace.into().into_owned()),
            prefix: prefix.map(|v| v.into().into_owned())
        }
    }

    /// Returns an optional prefix by reference, equivalent to `self.borrow().prefix`
    /// but avoids extra work.
    #[inline]
    pub fn prefix_as_ref(&self) -> Option<&str> {
        self.prefix.borrow_internals()
    }

    /// Returns an optional namespace by reference, equivalen to `self.borrow().namespace`
    /// but avoids extra work.
    #[inline]
    pub fn namespace_as_ref(&self) -> Option<&str> {
        self.namespace.borrow_internals()
    }

    /// See `Name::to_repr()` for details.
    #[inline]
    pub fn to_repr(&self) -> String {
        self.borrow().to_repr()
    }
}

impl FromStr for OwnedName {
    type Err = ();

    /// Parses the given string slice into a qualified name.
    ///
    /// This function, when finishes sucessfully, always return a qualified
    /// name without a namespace (`name.namespace == None`). It should be filled later
    /// using proper `NamespaceStack`.
    ///
    /// It is supposed that all characters in the argument string are correct
    /// as defined by the XML specification. No additional checks except a check
    /// for emptiness are done.
    fn from_str(s: &str) -> Result<OwnedName, ()> {
        let mut it = s.split(':');

        let r = match (it.next(), it.next(), it.next()) {
            (Some(prefix), Some(local_name), None) if !prefix.is_empty() &&
                                                      !local_name.is_empty() =>
                Some((local_name.to_owned(), Some(prefix.to_owned()))),
            (Some(local_name), None, None) if !local_name.is_empty() =>
                Some((local_name.to_owned(), None)),
            (_, _, _) => None
        };
        r.map(|(local_name, prefix)| OwnedName {
            local_name: local_name,
            namespace: None,
            prefix: prefix
        }).ok_or(())
    }
}

#[cfg(test)]
mod tests {
    use super::OwnedName;

    #[test]
    fn test_owned_name_from_str() {
        assert_eq!("prefix:name".parse(), Ok(OwnedName {
            local_name: "name".to_string(),
            namespace: None,
            prefix: Some("prefix".to_string())
        }));

        assert_eq!("name".parse(), Ok(OwnedName {
            local_name: "name".to_string(),
            namespace: None,
            prefix: None
        }));

        assert_eq!("".parse(), Err::<OwnedName, ()>(()));
        assert_eq!(":".parse(), Err::<OwnedName, ()>(()));
        assert_eq!(":a".parse(), Err::<OwnedName, ()>(()));
        assert_eq!("a:".parse(), Err::<OwnedName, ()>(()));
        assert_eq!("a:b:c".parse(), Err::<OwnedName, ()>(()));
    }
}
