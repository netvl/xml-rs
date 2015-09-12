use std::fmt;

use name::{Name, OwnedName};
use escape::escape_str_attribute;

/// A borrowed version of an XML attribute.
///
/// Consists of a borrowed qualified name and a borrowed string value.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Attribute<'a> {
    /// Attribute name.
    pub name: Name<'a>,

    /// Attribute value.
    pub value: &'a str
}

impl<'a> fmt::Display for Attribute<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}=\"{}\"", self.name, escape_str_attribute(self.value))
    }
}

impl<'a> Attribute<'a> {
    #[inline]
    pub fn to_owned(&self) -> OwnedAttribute {
        OwnedAttribute {
            name: self.name.into(),
            value: self.value.into()
        }
    }

    #[inline]
    pub fn new(name: Name<'a>, value: &'a str) -> Attribute<'a> {
        Attribute {
            name: name,
            value: value
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct OwnedAttribute {
    pub name: OwnedName,
    pub value: String
}

impl OwnedAttribute {
    pub fn borrow(&self) -> Attribute {
        Attribute {
            name: self.name.borrow(),
            value: &*self.value
        }
    }

    #[inline]
    pub fn new<S: Into<String>>(name: OwnedName, value: S) -> OwnedAttribute {
        OwnedAttribute {
            name: name,
            value: value.into()
        }
    }
}

impl fmt::Display for OwnedAttribute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}=\"{}\"", self.name, escape_str_attribute(&*self.value))
    }
}

#[cfg(test)]
mod tests {
    use super::{Attribute};

    use name::Name;

    #[test]
    fn attribute_show() {
        let attr = Attribute::new(
            Name::qualified("attribute", "urn:namespace", Some("n")),
            "its value with > & \" ' < weird symbols"
        );

        assert_eq!(
            &*attr.to_string(),
            "{urn:namespace}n:attribute=\"its value with &gt; &amp; &quot; &apos; &lt; weird symbols\""
        )
    }
}
