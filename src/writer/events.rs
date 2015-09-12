//! Contains `XmlEvent` datatype, instances of which are consumed by the emitter.

use std::borrow::Cow;

use name::Name;
use attribute::Attribute;
use common::XmlVersion;
use namespace::{Namespace, NS_NO_PREFIX};

/// A part of an XML output stream.
///
/// Objects of this enum are consumed by `EventWriter`. They correspond to different parts of
/// an XML document.
pub enum XmlEvent<'a> {
    /// Corresponds to XML document declaration.
    ///
    /// This event should always be written before any other event. If it is not written
    /// at all, a default XML declaration will be outputted if the corresponding option
    /// is set in the configuration. Otherwise an error will be returned.
    StartDocument {
        /// XML version.
        ///
        /// Defaults to `XmlVersion::Version10`.
        version: XmlVersion,

        /// XML document encoding.
        ///
        /// Defaults to `Some("UTF-8")`.
        encoding: Option<&'a str>,

        /// XML standalone declaration.
        ///
        /// Defaults to `None`.
        standalone: Option<bool>
    },

    /// Denotes an XML processing instruction.
    ProcessingInstruction {
        /// Processing instruction target.
        name: &'a str,

        /// Processing instruction content.
        data: Option<&'a str>
    },

    /// Denotes a beginning of an XML element.
    StartElement {
        /// Qualified name of the element.
        name: Name<'a>,

        /// A list of attributes associated with the element.
        ///
        /// Currently attributes are not checked for duplicates (TODO). Attribute values
        /// will be escaped, and all characters invalid for attribute values like `"` or `<`
        /// will be changed into character entities.
        attributes: Cow<'a, [Attribute<'a>]>,

        /// Contents of the namespace mapping at this point of the document.
        ///
        /// This mapping will be inspected for "new" entries, and if at this point of the document
        /// a particular pair of prefix and namespace URI is already defined, no namespace
        /// attributes will be emitted.
        namespace: Cow<'a, Namespace>,
    },

    /// Denotes an end of an XML element.
    EndElement {
        /// Optional qualified name of the element.
        ///
        /// If `None`, then it is assumed that the element name should be the last valid one.
        /// If `Some` and element names tracking is enabled, then the writer will check it for
        /// correctness.
        name: Option<Name<'a>>
    },

    /// Denotes CDATA content.
    ///
    /// This event contains unparsed data, and no escaping will be performed when writing it
    /// to the output stream.
    CData(&'a str),

    /// Denotes a comment.
    ///
    /// The string will be checked for invalid sequences and error will be returned by the
    /// write operation
    Comment(&'a str),

    /// Denotes character data outside of tags.
    ///
    /// Contents of this event will be escaped if `perform_escaping` option is enabled,
    /// that is, every character invalid for PCDATA will appear as a character entity.
    Characters(&'a str)
}

impl<'a> XmlEvent<'a> {
    #[inline]
    pub fn processing_instruction(name: &'a str, data: Option<&'a str>) -> XmlEvent<'a> {
        XmlEvent::ProcessingInstruction { name: name, data: data }
    }

    #[inline]
    pub fn start_element<S>(name: S) -> StartElementBuilder<'a> where S: Into<Name<'a>> {
        StartElementBuilder {
            name: name.into(),
            attributes: Vec::new(),
            namespace: Namespace::empty().into()
        }
    }

    #[inline]
    pub fn end_element() -> EndElementBuilder<'a> {
        EndElementBuilder { name: None }
    }

    #[inline]
    pub fn cdata(data: &'a str) -> XmlEvent<'a> { XmlEvent::CData(data) }

    #[inline]
    pub fn characters(data: &'a str) -> XmlEvent<'a> { XmlEvent::Characters(data) }

    #[inline]
    pub fn comment(data: &'a str) -> XmlEvent<'a> { XmlEvent::Comment(data) }
}

impl<'a> From<&'a str> for XmlEvent<'a> {
    #[inline]
    fn from(s: &'a str) -> XmlEvent<'a> { XmlEvent::Characters(s) }
}

pub struct EndElementBuilder<'a> {
    name: Option<Name<'a>>
}

impl<'a> EndElementBuilder<'a> {
    #[inline]
    pub fn name<N>(mut self, name: N) -> EndElementBuilder<'a> where N: Into<Name<'a>> {
        self.name = Some(name.into());
        self
    }
}

impl<'a> From<EndElementBuilder<'a>> for XmlEvent<'a> {
    fn from(b: EndElementBuilder<'a>) -> XmlEvent<'a> {
        XmlEvent::EndElement { name: b.name }
    }
}

pub struct StartElementBuilder<'a> {
    name: Name<'a>,
    attributes: Vec<Attribute<'a>>,
    namespace: Namespace
}

impl<'a> StartElementBuilder<'a> {
    #[inline]
    pub fn attr<N>(mut self, name: N, value: &'a str) -> StartElementBuilder<'a>
        where N: Into<Name<'a>>
    {
        self.attributes.push(Attribute::new(name.into(), value));
        self
    }

    #[inline]
    pub fn ns<S1, S2>(mut self, prefix: S1, uri: S2) -> StartElementBuilder<'a>
        where S1: Into<String>, S2: Into<String>
    {
        self.namespace.put(prefix, uri);
        self
    }

    #[inline]
    pub fn default_ns<S>(mut self, uri: S) -> StartElementBuilder<'a>
        where S: Into<String>
    {
        self.namespace.put(NS_NO_PREFIX, uri);
        self
    }
}

impl<'a> From<StartElementBuilder<'a>> for XmlEvent<'a> {
    #[inline]
    fn from(b: StartElementBuilder<'a>) -> XmlEvent<'a> {
        XmlEvent::StartElement {
            name: b.name,
            attributes: Cow::Owned(b.attributes),
            namespace: Cow::Owned(b.namespace)
        }
    }
}
