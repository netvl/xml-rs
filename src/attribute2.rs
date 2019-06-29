use std::borrow::Cow;
use std::fmt;

use crate::name2::Name;
use crate::writer::escape::escape_str_attribute;

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
    /// Creates a borrowed attribute using the provided borrowed name and a borrowed string value.
    #[inline]
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

    use crate::name2::Name;

    #[test]
    fn attribute_display() {
        let attr = Attribute::new(
            Name::qualified("attribute", "urn:namespace", Some("n")),
            "its value with > & \" ' < weird symbols",
        );

        assert_eq!(
            &*attr.to_string(),
            "{urn:namespace}n:attribute=\"its value with &gt; &amp; &quot; &apos; &lt; weird symbols\""
        )
    }
}
