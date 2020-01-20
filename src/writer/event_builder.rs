use std::borrow::Cow;

use crate::attribute::Attribute;
use crate::name::Name;
use crate::namespace::Namespace;
use crate::{namespace, Event};

pub struct EventBuilder;

impl EventBuilder {
    /// Returns a builder for a start element event.
    ///
    /// This builder can then be used to tweak attributes and namespace starting at
    /// this element.
    pub fn start_element<'a>(name: impl Into<Name<'a>>) -> StartElementBuilder<'a> {
        StartElementBuilder {
            name: name.into(),
            attributes: Vec::new(),
            namespace: Namespace::empty(),
        }
    }

    pub fn end_element<'a>() -> EndElementBuilder<'a> {
        EndElementBuilder { name: Name::EMPTY }
    }
}

pub struct EndElementBuilder<'a> {
    name: Name<'a>,
}

impl<'a> From<EndElementBuilder<'a>> for Event<'a> {
    fn from(builder: EndElementBuilder<'a>) -> Self {
        Event::end_element(builder.name)
    }
}

impl<'a> EndElementBuilder<'a> {
    /// Sets the name of this closing element.
    ///
    /// Usually the writer is able to determine closing element names automatically. If
    /// this functionality is enabled (by default it is), then this name is checked for correctness.
    /// It is possible, however, to disable such behavior; then the user must ensure that
    /// closing element name is correct manually.
    pub fn name<N>(mut self, name: impl Into<Name<'a>>) -> EndElementBuilder<'a> {
        self.name = name.into();
        self
    }
}

/// A builder for a start element event.
pub struct StartElementBuilder<'a> {
    name: Name<'a>,
    attributes: Vec<Attribute<'a>>,
    namespace: Namespace,
}

impl<'a> From<StartElementBuilder<'a>> for Event<'a> {
    fn from(builder: StartElementBuilder<'a>) -> Self {
        Event::start_element(builder.name, builder.attributes, builder.namespace)
    }
}

impl<'a> StartElementBuilder<'a> {
    /// Sets an attribute value of this element to the given string.
    ///
    /// This method can be used to add attributes to the starting element. Name is a qualified
    /// name; its namespace is ignored, but its prefix is checked for correctness, that is,
    /// it is checked that the prefix is bound to some namespace in the current context.
    ///
    /// Currently attributes are not checked for duplicates. Note that duplicate attributes
    /// are a violation of XML document well-formedness.
    ///
    /// The writer checks that you don't specify reserved prefix names, for example `xmlns`.
    pub fn attr(mut self, name: impl Into<Name<'a>>, value: impl Into<Cow<'a, str>>) -> StartElementBuilder<'a> {
        self.attributes.push(Attribute::new(name.into(), value.into()));
        self
    }

    /// Adds a namespace to the current namespace context.
    ///
    /// If no namespace URI was bound to the provided prefix at this point of the document,
    /// then the mapping from the prefix to the provided namespace URI will be written as
    /// a part of this element attribute set.
    ///
    /// If the same namespace URI was bound to the provided prefix at this point of the document,
    /// then no namespace attributes will be emitted.
    ///
    /// If some other namespace URI was bound to the provided prefix at this point of the document,
    /// then another binding will be added as a part of this element attribute set, shadowing
    /// the outer binding.
    pub fn ns(mut self, prefix: impl Into<String>, uri: impl Into<String>) -> StartElementBuilder<'a> {
        self.namespace.put(prefix, uri);
        self
    }

    /// Adds a default namespace mapping to the current namespace context.
    ///
    /// Same rules as for `ns()` are also valid for the default namespace mapping.
    pub fn default_ns<S>(mut self, uri: impl Into<String>) -> StartElementBuilder<'a> {
        self.namespace.put(namespace::NS_NO_PREFIX, uri);
        self
    }
}
