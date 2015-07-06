use name::Name;
use attribute::Attribute;
use common::XmlVersion;
use namespace::Namespace;

/// An element of an XML output stream.
///
/// Items of this enum are consumed by `writer::EventWriter`. They correspond to different
/// elements of an XML document.
pub enum XmlEvent<'a> {
    /// Corresponds to XML document declaration.
    ///
    /// This event should always be written before any other event. If it is not written
    /// at all, a default XML declaration will be outputted.
    StartDocument {
        /// XML version.
        ///
        /// Defaults to `Version10`.
        version: XmlVersion,

        /// XML document encoding.
        ///
        /// Defaults to `Some("UTF-8")`.
        encoding: Option<&'a str>,

        /// XML standalone declaration.
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
        /// Currently attributes are not checked for duplicates (TODO).
        attributes: Vec<Attribute<'a>>,

        /// Contents of the namespace mapping at this point of the document.
        namespace: &'a Namespace,
    },

    /// Denotes an end of an XML element.
    EndElement {
        /// Qualified name of the element.
        name: Name<'a>
    },

    /// Denotes CDATA content.
    ///
    /// This event contains unparsed data. No unescaping will be performed.
    ///
    /// It is possible to configure a parser to emit `Characters` event instead of `CData`. See
    /// `reader::ParserConfiguration` structure for more information.
    CData(&'a str),

    /// Denotes a comment.
    Comment(&'a str),

    /// Denotes character data outside of tags.
    ///
    /// Contents of this event will always be unescaped, so no entities like `&lt;` or `&amp;` or 
    /// `&#123;` will appear in it.
    Characters(&'a str)
}
