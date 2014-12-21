use std::fmt;
use std::str::FromStr;

use util::{OptionBorrowExt, IntoOwned};

#[deriving(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Name<'a> {
    pub local_name: &'a str,
    pub namespace: Option<&'a str>,
    pub prefix: Option<&'a str>
}

impl<'a> fmt::Show for Name<'a> {
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
    pub fn to_owned(&self) -> OwnedName {
        OwnedName {
            local_name: self.local_name.into_string(),
            namespace: self.namespace.map(|s| s.into_string()),
            prefix: self.prefix.map(|s| s.into_string())
        }
    }

    /// Returns a `Name` instance representing plain local name.
    #[inline]
    pub fn local(local_name: &str) -> Name {
        Name {
            local_name: local_name,
            prefix: None,
            namespace: None
        }
    }

    /// Returns a `Name` instance representing a qualified name with or without a prefix and
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
    /// This method is different from autoderived `to_string()` because it does not
    /// include namespace URI in the result.
    pub fn to_repr(&self) -> String {
        match self.prefix {
            Some(prefix) => format!("{}:{}", prefix, self.local_name),
            None => self.local_name.into_string()
        }
    }
}

#[deriving(Clone, PartialEq, Eq, Hash)]
pub struct OwnedName {
    pub local_name: String,
    pub namespace: Option<String>,
    pub prefix: Option<String>,
}

impl fmt::Show for OwnedName {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.borrow().fmt(f)
    }
}

impl OwnedName {
    pub fn borrow(&self) -> Name {
        Name {
            local_name: &*self.local_name,
            namespace: self.namespace.as_ref().map(|s| &**s),
            prefix: self.prefix.as_ref().map(|s| &**s),
        }
    }

    #[inline]
    pub fn local<S: IntoOwned<String>>(local_name: S) -> OwnedName {
        OwnedName {
            local_name: local_name.into_owned(),
            namespace: None,
            prefix: None,
        }
    }

    #[inline]
    pub fn qualified<S1, S2, S3>(local_name: S1, namespace: S2, prefix: Option<S3>) -> OwnedName
            where S1: IntoOwned<String>,
                  S2: IntoOwned<String>,
                  S3: IntoOwned<String> {
        OwnedName {
            local_name: local_name.into_owned(),
            namespace: Some(namespace.into_owned()),
            prefix: prefix.map(|v| v.into_owned())
        }
    }

    #[inline]
    pub fn prefix_as_ref(&self) -> Option<&str> {
        self.prefix.borrow_internals()
    }

    pub fn namespace_as_ref(&self) -> Option<&str> {
        self.namespace.borrow_internals()
    }

    #[inline]
    pub fn to_repr(&self) -> String {
        self.borrow().to_repr()
    }
}

impl FromStr for OwnedName {
    /// Parses the given string slice into a qualified name.
    ///
    /// This function, when finishes sucessfully, always return a qualified
    /// name without a namespace (`name.namespace == None`). It should be filled later
    /// using proper `NamespaceStack`.
    ///
    /// It is supposed that all characters in the argument string are correct
    /// as defined by the XML specification. No additional checks except a check
    /// for emptiness are done.
    fn from_str(s: &str) -> Option<OwnedName> {
        let mut it = s.split(':');

        let r = match (it.next(), it.next(), it.next()) {
            (Some(prefix), Some(local_name), None) if !prefix.is_empty() &&
                                                      !local_name.is_empty() =>
                Some((local_name.into_string(), Some(prefix.into_string()))),
            (Some(local_name), None, None) if !local_name.is_empty() =>
                Some((local_name.into_string(), None)),
            (_, _, _) => None
        };
        r.map(|(local_name, prefix)| OwnedName {
            local_name: local_name,
            namespace: None,
            prefix: prefix
        })
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::OwnedName;

    #[test]
    fn test_owned_name_from_str() {
        assert_eq!(FromStr::from_str("prefix:name"), Some(OwnedName {
            local_name: "name".into_string(),
            namespace: None,
            prefix: Some("prefix".into_string())
        }));

        assert_eq!(FromStr::from_str("name"), Some(OwnedName {
            local_name: "name".into_string(),
            namespace: None,
            prefix: None
        }));

        assert_eq!(FromStr::from_str(""), None::<OwnedName>);
        assert_eq!(FromStr::from_str(":"), None::<OwnedName>);
        assert_eq!(FromStr::from_str(":a"), None::<OwnedName>);
        assert_eq!(FromStr::from_str("a:"), None::<OwnedName>);
        assert_eq!(FromStr::from_str("a:b:c"), None::<OwnedName>);
    }
}
