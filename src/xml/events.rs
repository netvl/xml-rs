use common::{Name, Attribute, XmlVersion};

pub enum XmlEvent {
    StartDocument {
        version: XmlVersion,
        encoding: ~str,
        standalone: Option<bool>
    },
    EndDocument,
    Comment(~str),
    ProcessingInstruction { 
        name: Name, 
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
    Characters(~str),
    Whitespace(~str),
    Error {
        row: uint,
        col: uint,
        msg: ~str
    }
}


pub enum Standalone { SDYes, SDNo }

