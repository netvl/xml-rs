use std::borrow::Cow;
use std::fmt;

use name2::Name;
use attribute2::Attribute;

/// XML version enumeration.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum XmlVersion {
    /// XML version 1.0.
    Version10,

    /// XML version 1.1.
    Version11
}

impl fmt::Display for XmlVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            XmlVersion::Version10 => write!(f, "1.0"),
            XmlVersion::Version11 => write!(f, "1.1")
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum XmlEvent<'a> {
    StartDocument {
        version: XmlVersion,
        encoding: Cow<'a, str>,
        standalone: Option<bool>,
    },

    EndDocument,

    DoctypeDeclaration {
        content: Cow<'a, str>,
    },

    ProcessingInstruction {
        name: Cow<'a, str>,
        data: Option<Cow<'a, str>>,
    },

    StartElement {
        name: Name<'a>,
        // TODO: consider using SmallVec
        attributes: Vec<Attribute<'a>>,
    },

    EndElement {
        name: Name<'a>,
    },

    CData(Cow<'a, str>),

    Comment(Cow<'a, str>),

    Text(Cow<'a, str>),

    Whitespace(Cow<'a, str>),
}
