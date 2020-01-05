use derive_more::From;

use crate::event::{XmlEvent as ReifiedEvent, XmlVersion};
use crate::reader::model::attribute::Attribute;
use crate::reader::model::buffer::{BufSlice, Buffer};
use crate::reader::model::name::Name;

#[derive(Debug, Clone)]
pub enum Event {
    StartDocument {
        version: XmlVersion,
        encoding: BufSlice,
        standalone: Option<bool>,
    },

    EndDocument,

    DoctypeDeclaration {
        content: BufSlice,
    },

    ProcessingInstruction {
        name: BufSlice,
        data: Option<BufSlice>,
    },

    StartElement {
        name: Name,
        // TODO: consider using SmallVec
        attributes: Vec<Attribute>,
    },

    EndElement {
        name: Name,
    },

    CData(BufSlice),

    Comment(BufSlice),

    Text(BufSlice),

    Whitespace(BufSlice),
}

impl Event {
    pub fn start_document(version: XmlVersion, encoding: impl Into<BufSlice>, standalone: Option<bool>) -> Event {
        Event::StartDocument {
            version,
            encoding: encoding.into(),
            standalone,
        }
    }

    pub fn end_document() -> Event {
        Event::EndDocument
    }

    pub fn doctype_declaration(content: impl Into<BufSlice>) -> Event {
        Event::DoctypeDeclaration {
            content: content.into(),
        }
    }

    pub fn processing_instruction(name: impl Into<BufSlice>, data: Option<impl Into<BufSlice>>) -> Event {
        Event::ProcessingInstruction {
            name: name.into(),
            data: data.map(Into::into),
        }
    }

    pub fn start_element(name: Name, attributes: impl IntoIterator<Item = Attribute>) -> Event {
        Event::StartElement {
            name,
            attributes: attributes.into_iter().collect(),
        }
    }

    pub fn end_element(name: Name) -> Event {
        Event::EndElement { name }
    }

    pub fn cdata(data: impl Into<BufSlice>) -> Event {
        Event::CData(data.into())
    }

    pub fn comment(data: impl Into<BufSlice>) -> Event {
        Event::Comment(data.into())
    }

    pub fn text(data: impl Into<BufSlice>) -> Event {
        Event::Text(data.into())
    }

    pub fn whitespace(data: impl Into<BufSlice>) -> Event {
        Event::Whitespace(data.into())
    }

    pub fn as_reified<'buf>(&self, buffer: &'buf Buffer) -> ReifiedEvent<'buf> {
        match *self {
            Event::StartDocument {
                version,
                ref encoding,
                standalone,
            } => ReifiedEvent::start_document(version, encoding.as_reified(buffer), standalone),
            Event::EndDocument => ReifiedEvent::end_document(),
            Event::DoctypeDeclaration { content } => ReifiedEvent::doctype_declaration(content.as_reified(buffer)),
            Event::ProcessingInstruction { name, data } => {
                ReifiedEvent::processing_instruction(name.as_reified(buffer), data.map(|d| d.as_reified(buffer)))
            }
            Event::StartElement { name, ref attributes } => ReifiedEvent::start_element(
                name.as_reified(buffer),
                attributes.iter().cloned().map(|a| a.as_reified(buffer)),
            ),
            Event::EndElement { name } => ReifiedEvent::end_element(name.as_reified(buffer)),
            Event::CData(data) => ReifiedEvent::cdata(data.as_reified(buffer)),
            Event::Comment(data) => ReifiedEvent::comment(data.as_reified(buffer)),
            Event::Text(data) => ReifiedEvent::text(data.as_reified(buffer)),
            Event::Whitespace(data) => ReifiedEvent::whitespace(data.as_reified(buffer)),
        }
    }
}

#[derive(Debug, Clone, From)]
pub enum CowEvent {
    Ephemeral(Event),
    Reified(ReifiedEvent<'static>),
}

impl CowEvent {
    pub fn reified<'buf>(self, buffer: &'buf Buffer) -> ReifiedEvent<'buf> {
        match self {
            CowEvent::Ephemeral(e) => e.as_reified(buffer),
            CowEvent::Reified(e) => e,
        }
    }
}
