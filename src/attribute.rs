use std::borrow::Cow;
use std::fmt;

use crate::name::Name;
use crate::utils::escape::escape_str_attribute;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Attribute<'a> {
    pub name: Name<'a>,
    pub value: Cow<'a, str>,
}

impl<'a> fmt::Display for Attribute<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}=\"{}\"", self.name, escape_str_attribute(&self.value))
    }
}

impl<'a> Attribute<'a> {
    pub fn into_owned(self) -> Attribute<'static> {
        Attribute {
            name: self.name.into_owned(),
            value: self.value.into_owned().into(),
        }
    }

    /// Creates a borrowed attribute using the provided borrowed name and a borrowed string value.
    pub fn new(name: Name<'a>, value: impl Into<Cow<'a, str>>) -> Attribute<'a> {
        Attribute {
            name,
            value: value.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Attribute;

    use crate::name::Name;

    #[test]
    fn attribute_display() {
        let attr = Attribute::new(
            Name::prefixed("attribute", "n"),
            "its value with > & \" ' < weird symbols",
        );

        assert_eq!(
            &*attr.to_string(),
            "n:attribute=\"its value with &gt; &amp; &quot; &apos; &lt; weird symbols\""
        )
    }
}
