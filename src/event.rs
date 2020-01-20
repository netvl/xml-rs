use std::borrow::Cow;
use std::fmt;

use crate::attribute::Attribute;
use crate::name::Name;
use crate::namespace::{Namespace, NamespaceStack};

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
pub enum Event<'a> {
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
        namespace: Cow<'a, Namespace>,
    },

    EndElement {
        name: Name<'a>,
    },

    CData(Cow<'a, str>),

    Comment(Cow<'a, str>),

    Text(Cow<'a, str>),

    Whitespace(Cow<'a, str>),
}

impl<'a> Event<'a> {
    pub fn into_owned(self) -> Event<'static> {
        match self {
            Event::StartDocument {
                version,
                encoding,
                standalone,
            } => Event::start_document(version, encoding.into_owned(), standalone),
            Event::EndDocument => Event::EndDocument,
            Event::DoctypeDeclaration { content } => Event::doctype_declaration(content.into_owned()),
            Event::ProcessingInstruction { name, data } => {
                Event::processing_instruction(name.into_owned(), data.map(Cow::into_owned))
            }
            Event::StartElement {
                name,
                attributes,
                namespace,
            } => Event::start_element(
                name.into_owned(),
                attributes.into_iter().map(Attribute::into_owned),
                namespace.into_owned(),
            ),
            Event::EndElement { name } => Event::end_element(name.into_owned()),
            Event::CData(data) => Event::cdata(data.into_owned()),
            Event::Comment(data) => Event::comment(data.into_owned()),
            Event::Text(data) => Event::text(data.into_owned()),
            Event::Whitespace(data) => Event::whitespace(data.into_owned()),
        }
    }

    pub fn start_document(
        version: XmlVersion,
        encoding: impl Into<Cow<'a, str>>,
        standalone: Option<bool>,
    ) -> Event<'a> {
        Event::StartDocument {
            version,
            encoding: encoding.into(),
            standalone,
        }
    }

    pub fn end_document() -> Event<'a> {
        Event::EndDocument
    }

    pub fn doctype_declaration(content: impl Into<Cow<'a, str>>) -> Event<'a> {
        Event::DoctypeDeclaration {
            content: content.into(),
        }
    }

    pub fn processing_instruction(name: impl Into<Cow<'a, str>>, data: Option<impl Into<Cow<'a, str>>>) -> Event<'a> {
        Event::ProcessingInstruction {
            name: name.into(),
            data: data.map(Into::into),
        }
    }

    pub fn start_element(
        name: Name<'a>,
        attributes: impl IntoIterator<Item = Attribute<'a>>,
        namespace: impl Into<Cow<'a, Namespace>>,
    ) -> Event<'a> {
        Event::StartElement {
            name,
            attributes: attributes.into_iter().collect(),
            namespace: namespace.into(),
        }
    }

    pub fn end_element(name: Name<'a>) -> Event<'a> {
        Event::EndElement { name }
    }

    pub fn cdata(data: impl Into<Cow<'a, str>>) -> Event<'a> {
        Event::CData(data.into())
    }

    pub fn comment(data: impl Into<Cow<'a, str>>) -> Event<'a> {
        Event::Comment(data.into())
    }

    pub fn text(data: impl Into<Cow<'a, str>>) -> Event<'a> {
        Event::Text(data.into())
    }

    pub fn whitespace(data: impl Into<Cow<'a, str>>) -> Event<'a> {
        Event::Whitespace(data.into())
    }

    pub fn as_text_mut(&mut self) -> &mut Cow<'a, str> {
        match self {
            Event::Text(data) => data,
            _ => panic!("Event is not text"),
        }
    }

    pub fn as_text(&'a self) -> &'a str {
        match self {
            Event::Text(data) => data.as_ref(),
            _ => panic!("Event is not text"),
        }
    }

    // TODO: add error handling
    pub fn resolve_namespaces(&mut self, namespaces: &NamespaceStack) {
        match self {
            Event::StartElement {
                name,
                attributes,
                namespace,
            } => {
                name.resolve_namespace(namespaces);
                for attribute in attributes {
                    attribute.name.resolve_namespace(namespaces);
                }
                let topmost_level = namespaces.peek();
                if !topmost_level.is_essentially_empty() {
                    let namespace = namespace.to_mut();
                    for (k, v) in topmost_level.0.iter() {
                        namespace.put(k, v);
                    }
                }
            }
            Event::EndElement { name } => {
                name.resolve_namespace(namespaces);
            }
            _ => {}
        }
    }
}
