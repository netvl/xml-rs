//! Contains high-level interface for an events-based XML emitter.
//!
//! The most important type in this module is `EventWriter` which allows writing an XML document
//! to some output stream.

use std::io::prelude::*;

use crate::event::Event;

pub use self::config::WriterConfig;
pub use self::emitter::EmitterError as Error;
pub use self::emitter::Result;

use self::emitter::Emitter;

mod config;
mod emitter;

/// A wrapper around an `std::io::Write` instance which emits XML document according to provided
/// events.
pub struct Writer<W> {
    sink: W,
    emitter: Emitter,
}

impl<W: Write> Writer<W> {
    /// Creates a new `EventWriter` out of an `std::io::Write` instance using the default
    /// configuration.
    pub fn new(sink: W) -> Writer<W> {
        Writer::new_with_config(sink, WriterConfig::new())
    }

    /// Creates a new `EventWriter` out of an `std::io::Write` instance using the provided
    /// configuration.
    pub fn new_with_config(sink: W, config: WriterConfig) -> Writer<W> {
        Writer {
            sink,
            emitter: Emitter::new(config),
        }
    }

    /// Writes the next piece of XML document according to the provided event.
    ///
    /// Note that output data may not exactly correspond to the written event because
    /// of various configuration options. For example, `XmlEvent::EndElement` may
    /// correspond to a separate closing element or it may cause writing an empty element.
    /// Another example is that `XmlEvent::CData` may be represented as characters in
    /// the output stream.
    pub fn write<E>(&mut self, event: Event) -> Result<()> {
        match event.into() {
            Event::StartDocument {
                version,
                encoding,
                standalone,
            } => self
                .emitter
                .emit_start_document(&mut self.sink, version, &encoding, standalone),
            Event::DoctypeDeclaration { .. } => Ok(()), // TODO
            Event::EndDocument => Ok(()),               // TODO
            Event::ProcessingInstruction { name, data } => {
                self.emitter
                    .emit_processing_instruction(&mut self.sink, &name, data.as_deref())
            }
            Event::StartElement {
                name,
                attributes,
                //                namespace,
            } => {
                self.emitter.namespace_stack_mut().push_empty().checked_target();
                //                    .extend(namespace.as_ref());
                self.emitter.emit_start_element(&mut self.sink, name, &attributes)
            }
            Event::EndElement { name } => {
                let r = self.emitter.emit_end_element(&mut self.sink, Some(name));
                self.emitter.namespace_stack_mut().try_pop();
                r
            }
            Event::Comment(content) => self.emitter.emit_comment(&mut self.sink, &content),
            Event::CData(content) => self.emitter.emit_cdata(&mut self.sink, &content),
            Event::Text(content) => self.emitter.emit_characters(&mut self.sink, &content),
            Event::Whitespace(content) => self.emitter.emit_characters(&mut self.sink, &content),
        }
    }

    /// Returns a mutable reference to the underlying `Writer`.
    ///
    /// Note that having a reference to the underlying sink makes it very easy to emit invalid XML
    /// documents. Use this method with care. Valid use cases for this method include accessing
    /// methods like `Write::flush`, which do not emit new data but rather change the state
    /// of the stream itself.
    pub fn inner_mut(&mut self) -> &mut W {
        &mut self.sink
    }

    /// Unwraps this `EventWriter`, returning the underlying writer.
    ///
    /// Note that this is a destructive operation: unwrapping a writer and then wrapping
    /// it again with `EventWriter::new()` will create a fresh writer whose state will be
    /// blank; for example, accumulated namespaces will be reset.
    pub fn into_inner(self) -> W {
        self.sink
    }
}
