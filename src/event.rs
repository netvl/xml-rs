use std::borrow::Cow;
use std::fmt;

use crate::attribute::Attribute;
use crate::name::Name;

/// XML version enumeration.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum XmlVersion {
    /// XML version 1.0.
    Version10,

    /// XML version 1.1.
    Version11,
}

impl fmt::Display for XmlVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            XmlVersion::Version10 => write!(f, "1.0"),
            XmlVersion::Version11 => write!(f, "1.1"),
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

impl<'a> XmlEvent<'a> {
    pub fn into_owned(self) -> XmlEvent<'static> {
        match self {
            XmlEvent::StartDocument {
                version,
                encoding,
                standalone,
            } => XmlEvent::start_document(version, encoding.into_owned(), standalone),
            XmlEvent::EndDocument => XmlEvent::EndDocument,
            XmlEvent::DoctypeDeclaration { content } => XmlEvent::doctype_declaration(content.into_owned()),
            XmlEvent::ProcessingInstruction { name, data } => {
                XmlEvent::processing_instruction(name.into_owned(), data.map(Cow::into_owned))
            }
            XmlEvent::StartElement { name, attributes } => {
                XmlEvent::start_element(name.into_owned(), attributes.into_iter().map(Attribute::into_owned))
            }
            XmlEvent::EndElement { name } => XmlEvent::end_element(name.into_owned()),
            XmlEvent::CData(data) => XmlEvent::cdata(data.into_owned()),
            XmlEvent::Comment(data) => XmlEvent::comment(data.into_owned()),
            XmlEvent::Text(data) => XmlEvent::text(data.into_owned()),
            XmlEvent::Whitespace(data) => XmlEvent::whitespace(data.into_owned()),
        }
    }

    pub fn start_document(
        version: XmlVersion,
        encoding: impl Into<Cow<'a, str>>,
        standalone: Option<bool>,
    ) -> XmlEvent<'a> {
        XmlEvent::StartDocument {
            version,
            encoding: encoding.into(),
            standalone,
        }
    }

    pub fn end_document() -> XmlEvent<'a> {
        XmlEvent::EndDocument
    }

    pub fn doctype_declaration(content: impl Into<Cow<'a, str>>) -> XmlEvent<'a> {
        XmlEvent::DoctypeDeclaration {
            content: content.into(),
        }
    }

    pub fn processing_instruction(
        name: impl Into<Cow<'a, str>>,
        data: Option<impl Into<Cow<'a, str>>>,
    ) -> XmlEvent<'a> {
        XmlEvent::ProcessingInstruction {
            name: name.into(),
            data: data.map(Into::into),
        }
    }

    pub fn start_element(name: Name<'a>, attributes: impl IntoIterator<Item = Attribute<'a>>) -> XmlEvent<'a> {
        XmlEvent::StartElement {
            name,
            attributes: attributes.into_iter().collect(),
        }
    }

    pub fn end_element(name: Name<'a>) -> XmlEvent<'a> {
        XmlEvent::EndElement { name }
    }

    pub fn cdata(data: impl Into<Cow<'a, str>>) -> XmlEvent<'a> {
        XmlEvent::CData(data.into())
    }

    pub fn comment(data: impl Into<Cow<'a, str>>) -> XmlEvent<'a> {
        XmlEvent::Comment(data.into())
    }

    pub fn text(data: impl Into<Cow<'a, str>>) -> XmlEvent<'a> {
        XmlEvent::Text(data.into())
    }

    pub fn whitespace(data: impl Into<Cow<'a, str>>) -> XmlEvent<'a> {
        XmlEvent::Whitespace(data.into())
    }

    pub fn as_text_mut(&mut self) -> &mut Cow<'a, str> {
        match self {
            XmlEvent::Text(data) => data,
            _ => panic!("Event is not text"),
        }
    }

    pub fn as_text(&'a self) -> &'a str {
        match self {
            XmlEvent::Text(data) => data.as_ref(),
            _ => panic!("Event is not text"),
        }
    }
}
