use std::io;
use std::io::prelude::*;
use std::fmt;
use std::result;

use common;
use name::Name;
use attribute::Attribute;
use escape::escape_str;
use common::XmlVersion;
use namespace::{NamespaceStack, UriMapping, NS_NO_PREFIX};

use writer::config::EmitterConfig;

#[derive(Debug)]
pub enum EmitterError {
    Io(io::Error),
    DocumentStartAlreadyEmitted,
    UnexpectedEvent
}

impl From<io::Error> for EmitterError {
    fn from(err: io::Error) -> EmitterError {
        EmitterError::Io(err)
    }
}

impl fmt::Display for EmitterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "emitter error: "));
        match *self {
            EmitterError::Io(ref e) => 
                write!(f, "I/O error: {}", e),
            EmitterError::DocumentStartAlreadyEmitted => 
                write!(f, "document start event has already been emitted"),
            EmitterError::UnexpectedEvent =>
                write!(f, "unexpected event")
        }
    }
}

pub type Result<T> = result::Result<T, EmitterError>;

pub struct Emitter {
    config: EmitterConfig,

    nst: NamespaceStack,

    indent_level: usize,
    indent_stack: Vec<IndentFlags>,

    start_document_emitted: bool
}

impl Emitter {
    pub fn new(config: EmitterConfig) -> Emitter {
        Emitter {
            config: config,

            nst: NamespaceStack::empty(),

            indent_level: 0,
            indent_stack: vec!(IndentFlags::empty()),

            start_document_emitted: false
        }
    }
}

macro_rules! try_chain(
    (ignore $e:expr) => (try!($e));
    ($e:expr) => (Ok(try!($e)));
    ($e:expr, $($rest:tt)*) => ({
        try!($e);
        try_chain!($($rest)*)
    })
);

macro_rules! wrapped_with(
    ($_self:ident; $before_name:ident ($arg:expr) and $after_name:ident, $body:expr) => ({
        try!($_self.$before_name($arg));
        let result = $body;
        $_self.$after_name();
        result
    })
);

macro_rules! if_present(
    ($opt:ident, $body:expr) => ($opt.map(|$opt| $body).unwrap_or(Ok(())))
);

bitflags!(
    flags IndentFlags: u8 {
        const WROTE_NOTHING = 0,
        const WROTE_MARKUP  = 1,
        const WROTE_TEXT    = 2
    }
);

impl Emitter {
    /// Returns current state of namespaces.
    #[inline]
    pub fn namespace_stack<'a>(&'a self) -> &'a NamespaceStack {
        &self.nst
    }

    #[inline]
    fn wrote_text(&self) -> bool {
        self.indent_stack.last().unwrap().contains(WROTE_TEXT)
    }

    #[inline]
    fn wrote_markup(&self) -> bool {
        self.indent_stack.last().unwrap().contains(WROTE_MARKUP)
    }

    #[inline]
    fn set_wrote_text(&mut self) {
        *self.indent_stack.last_mut().unwrap() = WROTE_TEXT;
    }

    #[inline]
    fn set_wrote_markup(&mut self) {
        *self.indent_stack.last_mut().unwrap() = WROTE_MARKUP;
    }

    #[inline]
    fn reset_state(&mut self) {
        *self.indent_stack.last_mut().unwrap() = WROTE_NOTHING;
    }

    fn write_newline<W: Write>(&mut self, target: &mut W, level: usize) -> Result<()> {
        try!(target.write(self.config.line_separator.as_bytes()));
        for _ in (0 .. level) {
            try!(target.write(self.config.indent_string.as_bytes()));
        }
        Ok(())
    }

    fn before_markup<W: Write>(&mut self, target: &mut W) -> Result<()> {
        if !self.wrote_text() && (self.indent_level > 0 || self.wrote_markup()) {
            let indent_level = self.indent_level;
            try!(self.write_newline(target, indent_level));
            if self.indent_level > 0 && self.config.indent_string.len() > 0 {
                self.after_markup();
            }
        }
        Ok(())
    }

    fn after_markup(&mut self) {
        self.set_wrote_markup();
    }

    fn before_start_element<W: Write>(&mut self, target: &mut W) -> Result<()> {
        try!(self.before_markup(target));
        self.indent_stack.push(WROTE_NOTHING);
        Ok(())
    }

    fn after_start_element(&mut self) {
        self.after_markup();
        self.indent_level += 1;
    }

    fn before_end_element<W: Write>(&mut self, target: &mut W) -> Result<()> {
        if self.indent_level > 0 && self.wrote_markup() && !self.wrote_text() {
            let indent_level = self.indent_level;
            self.write_newline(target, indent_level - 1)
        } else {
            Ok(())
        }
    }

    fn after_end_element(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
            self.indent_stack.pop();
        }
        self.set_wrote_markup();
    }

    fn after_text(&mut self) {
        self.set_wrote_text();
    }

    pub fn emit_start_document<W: Write>(&mut self, target: &mut W,
                                         version: XmlVersion,
                                         encoding: &str,
                                         standalone: Option<bool>) -> Result<()> {
        if self.start_document_emitted {
            return Err(EmitterError::DocumentStartAlreadyEmitted);
        }
        self.start_document_emitted = true;

        wrapped_with!(self; before_markup(target) and after_markup,
            try_chain! {
                write!(target, "<?xml version=\"{}\" encoding=\"{}\"", version, encoding),

                if_present!(standalone,
                            write!(target, " standalone=\"{}\"",
                                   if standalone { "yes" } else { "no" })),

                write!(target, "?>")
            }
        )
    }

    fn check_document_started<W: Write>(&mut self, target: &mut W) -> Result<()> {
        if !self.start_document_emitted && self.config.write_document_declaration {
            self.emit_start_document(target, common::XmlVersion::Version10, "utf-8", None)
        } else {
            Ok(())
        }
    }

    pub fn emit_processing_instruction<W: Write>(&mut self,
                                                 target: &mut W,
                                                 name: &str,
                                                 data: Option<&str>) -> Result<()> {
        try!(self.check_document_started(target));

        wrapped_with!(self; before_markup(target) and after_markup,
            try_chain! {
                write!(target, "<?{}", name),
                if_present!(data, write!(target, " {}", data)),
                write!(target, "?>")
            }
        )
    }

    fn emit_start_element_initial<'a, W, N>(&mut self, target: &mut W,
                                            name: Name,
                                            attributes: &[Attribute],
                                            namespace: &'a N) -> Result<()>
        where W: Write,
              &'a N: IntoIterator<Item=UriMapping<'a>>
    {
        
        try_chain! {
            self.check_document_started(target),
            self.before_start_element(target),
            write!(target, "<{}", name.to_repr()),
            self.emit_namespace_attributes(target, namespace),
            self.emit_attributes(target, attributes)
        }
    }

    pub fn emit_empty_element<'a, W, N>(&mut self, target: &mut W,
                                        name: Name,
                                        attributes: &[Attribute],
                                        namespace: &'a N) -> Result<()>
        where W: Write,
              &'a N: IntoIterator<Item=UriMapping<'a>>
    {
        try_chain! {
            self.emit_start_element_initial(target, name, attributes, namespace),
            write!(target, "/>")
        }

    }

    pub fn emit_start_element<'a, W, N>(&mut self, target: &mut W,
                                        name: Name,
                                        attributes: &[Attribute],
                                        namespace: &'a N) -> Result<()>
        where W: Write,
              &'a N: IntoIterator<Item=UriMapping<'a>>
    {
        try_chain! {
            self.emit_start_element_initial(target, name, attributes, namespace),
            write!(target, ">")
        }
    }

    pub fn emit_namespace_attributes<'a, W, N>(&mut self, target: &mut W,
                                               namespace: &'a N) -> Result<()>
        where W: Write,
              &'a N: IntoIterator<Item=UriMapping<'a>>
    {
        for (prefix, uri) in namespace.into_iter() {
            try!(match prefix {
                // internal namespaces are not emitted
                "xmlns" | "xml" => Ok(()),
                // there is already a namespace binding with this prefix in scope
                prefix if self.nst.get(prefix) == Some(uri) => Ok(()),  
                // emit xmlns only if it is overridden
                NS_NO_PREFIX => if !uri.is_empty() {  
                    write!(target, " xmlns=\"{}\"", uri)
                } else { Ok(()) },
                // everything else
                prefix => write!(target, " xmlns:{}=\"{}\"", prefix, uri)
            });
        }
        Ok(())
    }

    pub fn emit_attributes<W: Write>(&mut self, target: &mut W,
                                      attributes: &[Attribute]) -> Result<()> {
        for attr in attributes.iter() {
            try!(write!(target, " {}=\"{}\"", attr.name.to_repr(), escape_str(attr.value)))
        }
        Ok(())
    }

    pub fn emit_end_element<W: Write>(&mut self, target: &mut W,
                                       name: Name) -> Result<()> {
        wrapped_with!(self; before_end_element(target) and after_end_element,
            write!(target, "</{}>", name.to_repr()).map_err(From::from)
        )
    }

    pub fn emit_cdata<W: Write>(&mut self, target: &mut W, content: &str) -> Result<()> {
        if self.config.cdata_to_characters {
            self.emit_characters(target, content)
        } else {
            try_chain! {
                target.write(b"<![CDATA["),
                target.write(content.as_bytes()),
                ignore target.write(b"]]>")
            };
            self.after_text();
            Ok(())
        }
    }

    pub fn emit_characters<W: Write>(&mut self, target: &mut W,
                                      content: &str) -> Result<()> {
        try!(target.write(escape_str(content).as_bytes()));
        self.after_text();
        Ok(())
    }

    pub fn emit_comment<W: Write>(&mut self, target: &mut W, content: &str) -> Result<()> {
        Ok(())  // TODO: proper write
    }
}
