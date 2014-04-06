//! Contains `XmlEvent` datatype, instances of which are emitted by the parser.

use std::fmt;

use common;
use common::{Name, Namespace, Error, HasPosition, Attribute, XmlVersion};

/// An element of an XML stream.
///
/// Items of this enum are emitted by `pull::Parser`. They correspond to different
/// elements of an XML document.
#[deriving(Eq, Clone)]
pub enum XmlEvent {
    /// Corresponds to XML document declaration. 
    ///
    /// This event is always emitted before any other event (except `Error`). It is emitted
    /// even if the actual declaration is not present in the document.
    StartDocument {
        /// XML version.
        ///
        /// If XML declaration is not present, defaults to `Version10`.
        version: XmlVersion,

        /// XML document encoding.
        ///
        /// If XML declaration is not present or does not contain `encoding` attribute,
        /// defaults to `"UTF-8"`. This field is currently used for no other purpose than
        /// informational.
        encoding: ~str,

        /// XML standalone declaration.
        ///
        /// If XML document is not present or does not contain `standalone` attribute,
        /// defaults to `None`. This field is currently used for no other purpose than
        /// informational.
        standalone: Option<bool>
    },

    /// Denotes to the end of the document stream.
    ///
    /// This event is always emitted after any other event (except `Error`). After it 
    /// is emitted for the first time, it will always be emitted on next event pull attempts.
    EndDocument,

    /// Denotes an XML processing instruction.
    ///
    /// This event contains a processing instruction target (`name`) and opaque `data`. It
    /// is up to the application to process them.
    ProcessingInstruction { 
        /// Processing instruction target.
        name: ~str, 

        /// Processing instruction content.
        data: Option<~str> 
    },

    /// Denotes a beginning of an XML element.
    ///
    /// This event is emitted after parsing opening tags or after parsing bodiless tags. In the
    /// latter case `EndElement` event immediately follows.
    StartElement { 
        /// Qualified name of the element.
        name: Name,

        /// A list of attributes associated with the element.
        /// 
        /// Currently attributes are not checked for duplicates (TODO)
        attributes: Vec<Attribute>,

        /// Contents of the namespace mapping at this point of the document.
        namespace: Namespace,
    },

    /// Denotes an end of an XML document.
    ///
    /// This event is emitted after parsing closing tags or after parsing bodiless tags. In the
    /// latter case it is emitted immediately after corresponding `StartElement` event.
    EndElement {
        /// Qualified name of the element.
        name: Name
    },

    /// Denotes CDATA content.
    ///
    /// This event contains unparsed data. No unescaping will be performed.
    ///
    /// It is possible to configure a parser to emit `Characters` event instead of `CData`. See
    /// `pull::ParserConfiguration` structure for more information.
    CData(~str),

    /// Denotes a comment.
    ///
    /// It is possible to configure a parser to ignore comments, so this event will never be emitted.
    /// See `pull::ParserConfiguration` structure for more information.
    Comment(~str),

    /// Denotes character data outside of tags.
    ///
    /// Contents of this event will always be unescaped, so no entities like `&lt;` or `&amp;` or `&#123;`
    /// will appear in it.
    ///
    /// It is possible to configure a parser to trim leading and trailing whitespace for this event.
    /// See `pull::ParserConfiguration` structure for more information.
    Characters(~str),

    /// Denotes a chunk of whitespace outside of tags.
    ///
    /// It is possible to configure a parser to emit `Characters` event instead of `Whitespace`.
    /// See `pull::ParserConfiguration` structure for more information. When combined with whitespace
    /// trimming, it will eliminate standalone whitespace from the event stream completely.
    Whitespace(~str),

    /// Denotes parsing error.
    ///
    /// This event will always be the last event in the stream; no further XML processing will be done
    /// as is required by XML 1.1 specification, [section 1.2][1].
    ///
    /// [1]: http://www.w3.org/TR/2006/REC-xml11-20060816/#sec-terminology
    Error(common::Error)
}

impl fmt::Show for XmlEvent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            StartDocument { ref version, ref encoding, ref standalone } =>
                write!(f.buf, "StartDocument({:s}, {}, {})", version.to_str(), *encoding, *standalone),
            EndDocument =>
                write!(f.buf, "EndDocument"),
            ProcessingInstruction { ref name, ref data } =>
                write!(f.buf, "ProcessingInstruction({}{})", *name, match *data {
                    Some(ref data) => format!(", {:?}", *data),
                    None       => ~""
                }),
            StartElement { ref name, ref attributes, namespace: Namespace(ref namespace) } =>
                write!(f.buf, "StartElement({}, {}{})", name.to_str(), namespace.to_str(), if attributes.is_empty() {
                    ~""
                } else {
                    let attributes: Vec<~str> = attributes.iter().map(|a| format!("{} -> {:?}", a.name.to_str(), a.value)).collect();
                    format!(", [{}]", attributes.connect(", "))
                }),
            EndElement { ref name } =>
                write!(f.buf, "EndElement({})", name.to_str()),
            Comment(ref data) =>
                write!(f.buf, "Comment({:?})", *data),
            CData(ref data) =>
                write!(f.buf, "CData({:?})", *data),
            Characters(ref data) =>
                write!(f.buf, "Characters({:?})", *data),
            Whitespace(ref data) =>
                write!(f.buf, "Whitespace({:?})", *data),
            Error(ref e) =>
                write!(f.buf, "Error(row: {}, col: {}, message: {})", e.row()+1, e.col()+1, e.msg())
        }
    }
}
