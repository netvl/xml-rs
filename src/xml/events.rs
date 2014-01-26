use std::fmt;

use common;
use common::{Name, Error, HasPosition, Attribute, XmlVersion};

#[deriving(Eq, Clone)]
pub enum XmlEvent {
    StartDocument {
        version: XmlVersion,
        encoding: ~str,
        standalone: Option<bool>
    },
    EndDocument,
    ProcessingInstruction { 
        name: ~str, 
        data: Option<~str> 
    },
    StartElement { 
        name: Name,
        attributes: ~[Attribute]
    },
    EndElement {
        name: Name
    },
    CData(~str),
    Comment(~str),
    Characters(~str),
    Whitespace(~str),
    Error(common::Error)
}

impl fmt::Default for XmlEvent {
    fn fmt(ev: &XmlEvent, f: &mut fmt::Formatter) {
        match *ev {
            StartDocument { ref version, ref encoding, ref standalone } =>
                write!(f.buf, "StartDocument({:?}, {}, {})", *version, *encoding, *standalone),
            EndDocument =>
                write!(f.buf, "EndDocument"),
            ProcessingInstruction { ref name, ref data } =>
                write!(f.buf, "ProcessingInstruction({}{})", *name, match *data {
                    Some(ref data) => format!(", {}", *data),
                    None       => ~""
                }),
            StartElement { ref name, ref attributes } =>
                write!(f.buf, "StartElement({}{})", name.to_str(), if attributes.is_empty() {
                    ~""
                } else {
                    let attributes: ~[~str] = attributes.iter().map(|a| format!("{} -> {:?}", a.name.to_str(), a.value)).collect();
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
                write!(f.buf, "Error(row: {}, col: {}, message: {})", e.row(), e.col(), e.msg())
        }
    }
}
