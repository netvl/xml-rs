use std::borrow::Cow;

use arraydeque::ArrayDeque;
use nom::error::ParseError;
use nom::{Err, IResult};

use crate::event::XmlVersion;
use crate::namespace::{self, NamespaceStack};
use crate::reader::data::{BufSlice, Buffer};
use crate::reader::model;
use crate::utils::position::{Position, TextPosition};
use crate::{Event, ReaderConfig};

use self::parsers::{Parsed, ParsedHint, ReferenceHint, Span, StartTagHint};

mod parsers;
mod parsers2;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
enum State {
    Prolog(PrologSubstate),
    OutsideTag,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
enum PrologSubstate {
    BeforeDeclaration,
    BeforeDoctype,
    BeforeDocument,
}

#[derive(Debug)]
pub struct ParserLogicOutput {
    pub bytes_read: usize,
    pub pop_namespace: bool,
    pub events: ArrayDeque<[model::CowEvent; 3]>,
}

impl ParserLogicOutput {
    fn new(bytes_read: usize) -> ParserLogicOutput {
        ParserLogicOutput {
            bytes_read,
            pop_namespace: false,
            events: ArrayDeque::new(),
        }
    }

    fn push_front(&mut self, event: impl Into<model::CowEvent>) {
        self.events
            .push_front(event.into())
            .expect("Implementation error: too many events are pushed to the parser output");
    }

    fn push_back(&mut self, event: impl Into<model::CowEvent>) {
        self.events
            .push_back(event.into())
            .expect("Implementation error: too many events are pushed to the parser output");
    }

    pub fn into_iter(self) -> impl Iterator<Item = model::CowEvent> {
        self.events.into_iter()
    }
}

pub enum ParserLogicError<E> {
    Incomplete,
    Parsing(E),
    Logic(Cow<'static, str>),
}

impl<E> From<Err<E>> for ParserLogicError<E> {
    fn from(e: Err<E>) -> Self {
        match e {
            Err::Error(e) | Err::Failure(e) => ParserLogicError::Parsing(e),
            Err::Incomplete(_) => ParserLogicError::Incomplete,
        }
    }
}

impl<E> From<String> for ParserLogicError<E> {
    fn from(s: String) -> Self {
        ParserLogicError::Logic(s.into())
    }
}

impl<E> From<&'static str> for ParserLogicError<E> {
    fn from(s: &'static str) -> Self {
        ParserLogicError::Logic(s.into())
    }
}

type ParserLogicResult<E> = std::result::Result<ParserLogicOutput, ParserLogicError<E>>;

pub struct ParserLogic {
    config: ReaderConfig,
    state: State,
    encountered_declaration: bool,
    encountered_element: bool,
    open_elements: Vec<model::CowName>,
    namespaces: NamespaceStack,
    // TODO: NamespaceStack must also use CowNames
    pop_namespace_before_next_event: bool,
    position: TextPosition,
}

impl ParserLogic {
    pub fn new(config: ReaderConfig) -> ParserLogic {
        ParserLogic {
            config,
            state: State::Prolog(PrologSubstate::BeforeDeclaration),
            encountered_declaration: false,
            encountered_element: false,
            open_elements: Vec::new(),
            namespaces: NamespaceStack::default(),
            pop_namespace_before_next_event: false,
            position: TextPosition::START,
        }
    }

    pub fn is_document_correctly_closed(&self) -> bool {
        self.encountered_element && self.open_elements.is_empty()
    }

    pub fn pop_namespace_before_next_event(&mut self) {
        self.pop_namespace_before_next_event = true;
    }

    pub fn current_namespaces(&self) -> &NamespaceStack {
        &self.namespaces
    }

    pub fn reify_state(&mut self, buffer: &Buffer) {
        for name in &mut self.open_elements {
            name.reify_in_place(buffer);
        }
    }

    pub fn try_next<'buf, E>(&mut self, buffer: &'buf Buffer, input: &'buf str) -> ParserLogicResult<E>
    where
        E: ParseError<Span<'buf>>,
    {
        if self.pop_namespace_before_next_event {
            self.pop_namespace_before_next_event = false;
            self.namespaces.pop();
        }

        let input = Span::new(input);
        let result = match self.state {
            State::Prolog(substate) => self.parse_prolog(input, substate),
            State::OutsideTag => self.parse_outside_tag(input),
        };
        let (remainder, Parsed { event, hint, span }) = result?;
        self.position.advance_both(
            span.location_line().saturating_sub(1) as u64,
            span.get_utf8_column().saturating_sub(1) as u64,
        );

        let mut output = ParserLogicOutput::new(input.fragment().len() - remainder.fragment().len());

        let event = match event {
            model::Event::CData(data) if self.config.cdata_to_text => model::Event::text(data),
            model::Event::Whitespace(data) if self.config.whitespace_to_text => model::Event::text(data),
            other => other,
        };

        match event {
            event @ model::Event::StartDocument { .. } => {
                self.encountered_declaration = true;
                self.state = State::Prolog(PrologSubstate::BeforeDoctype);
                output.push_front(event);
            }

            event @ model::Event::DoctypeDeclaration { .. } => {
                self.state = State::Prolog(PrologSubstate::BeforeDocument);
                output.push_front(event);

                if !self.encountered_declaration {
                    self.encountered_declaration = true;
                    output.push_front(model::Event::start_document(
                        XmlVersion::Version10,
                        BufSlice::new_static("UTF-8"),
                        None,
                    ));
                }
            }

            mut event @ model::Event::StartElement { .. } => {
                match hint {
                    ParsedHint::StartTag(StartTagHint::EmptyElementTag) => {
                        output.push_back(model::Event::end_element(event.start_element_name()));
                    }
                    ParsedHint::StartTag(StartTagHint::RegularTag) => {
                        self.open_elements.push(event.start_element_name().into());
                    }
                    hint => {
                        debug_assert!(false, "Unexpected ParsedHint: {:?}", hint);
                    }
                }
                self.encountered_element = true;
                self.state = State::OutsideTag;

                self.namespaces.push_empty();

                // TODO: this "indices to remove" machinery can be probably replaced with parsed hints
                //       which would split the namespace-related attributes off directly during parsing
                let mut indices_to_remove = Vec::new();
                for (i, attribute) in event.attributes().iter().enumerate().rev() {
                    let attr_prefix = attribute.name.prefix.map(|p| p.as_reified(buffer));
                    let ns_prefix = attribute.name.local_name.as_reified(buffer);
                    let ns_uri = attribute.value.as_reified(buffer);
                    // Declaring a new prefix. It is sufficient to check the prefix only because the `xmlns`
                    // prefix is reserved.
                    if attr_prefix == Some(namespace::NS_XMLNS_PREFIX) {
                        if ns_prefix == namespace::NS_XMLNS_PREFIX {
                            return Err(format!("Cannot redefine the '{}' prefix", namespace::NS_XMLNS_PREFIX).into());
                        } else if ns_prefix == namespace::NS_XML_PREFIX && ns_uri != namespace::NS_XML_URI {
                            let msg = format!(
                                "Prefix '{}' cannot be re-bound to another URI",
                                namespace::NS_XML_PREFIX
                            );
                            return Err(msg.into());
                        } else if ns_uri.is_empty() {
                            return Err(format!("Cannot un-define prefix '{}'", ns_prefix).into());
                        } else {
                            indices_to_remove.push(i);
                            self.namespaces.put(ns_prefix, ns_uri);
                        }
                    // Declaring the default namespace
                    } else if attr_prefix.is_none() && ns_prefix == namespace::NS_XMLNS_PREFIX {
                        match ns_uri {
                            namespace::NS_XMLNS_URI | namespace::NS_XML_URI => {
                                return Err(format!("Namespace '{}' cannot be the default one", ns_uri).into());
                            }
                            _ => {
                                indices_to_remove.push(i);
                                self.namespaces.put(namespace::NS_NO_PREFIX, ns_uri);
                            }
                        }
                    }
                }
                // These count downwards, so no need to adjust them for removed elements
                for i in indices_to_remove {
                    event.attributes_mut().remove(i);
                }

                output.push_front(event);

                if !self.encountered_declaration {
                    self.encountered_declaration = true;
                    output.push_front(model::Event::start_document(
                        XmlVersion::Version10,
                        BufSlice::new_static("UTF-8"),
                        None,
                    ));
                }
            }

            event @ model::Event::EndElement { .. } => match self.open_elements.pop() {
                Some(open_name) => {
                    let open_name = open_name.as_reified(buffer);
                    let name = event.end_element_name().as_reified(buffer);
                    if name != open_name {
                        return Err(format!("Unexpected closing element '{}', expected '{}'", name, open_name).into());
                    }
                    output.push_front(event);
                }
                None => {
                    return Err("Unexpected closing element, expected an opening element".into());
                }
            },

            event @ model::Event::Comment(_) => {
                // A comment in the header means that no declaration must follow
                self.state = match self.state {
                    State::Prolog(PrologSubstate::BeforeDeclaration) => State::Prolog(PrologSubstate::BeforeDoctype),
                    other => other,
                };
                output.push_front(event);
            }

            event @ model::Event::ProcessingInstruction { .. } => {
                // A PI in the header means that no declaration must follow
                self.state = match self.state {
                    State::Prolog(PrologSubstate::BeforeDeclaration) => State::Prolog(PrologSubstate::BeforeDoctype),
                    other => other,
                };
                output.push_front(event);
            }

            event @ model::Event::CData(_) => {
                output.push_front(event);
            }

            event @ model::Event::Whitespace(_) => {
                output.push_front(event);
            }

            event @ model::Event::Text(_) => {
                match hint {
                    ParsedHint::Reference(ReferenceHint::Char(ch)) => {
                        output.push_front(Event::Text(ch.to_string().into()));
                    }
                    ParsedHint::Reference(ReferenceHint::Entity(name)) => {
                        // TODO: maybe we should move recognition of built-in entities into the parser below
                        let resolved = match name.as_reified(buffer).local_name.as_ref() {
                            "lt" => '<',
                            "gt" => '>',
                            "amp" => '&',
                            "apos" => '\'',
                            "quot" => '"',
                            r => return Err(format!("Unknown character reference: {}", r).into()),
                        };
                        output.push_front(Event::Text(resolved.to_string().into()));
                    }
                    ParsedHint::None => {
                        // Just regular text
                        output.push_front(event);
                    }
                    hint => {
                        debug_assert!(false, "Unexpected ParsedHint: {:?}", hint);
                    }
                }
            }

            other => todo!("Cannot handle yet: {:?}", other),
        }

        Ok(output)
    }

    fn parse_prolog<'buf, E>(
        &mut self,
        input: Span<'buf>,
        substate: PrologSubstate,
    ) -> IResult<Span<'buf>, Parsed<'buf>, E>
    where
        E: ParseError<Span<'buf>>,
    {
        match substate {
            PrologSubstate::BeforeDeclaration => parsers::before_declaration(input),
            PrologSubstate::BeforeDoctype => parsers::before_doctype(input),
            PrologSubstate::BeforeDocument => parsers::before_document(input),
        }
    }

    fn parse_outside_tag<'buf, E>(&mut self, input: Span<'buf>) -> IResult<Span<'buf>, Parsed<'buf>, E>
    where
        E: ParseError<Span<'buf>>,
    {
        parsers::outside_tag(input)
    }
}

impl Position for ParserLogic {
    fn position(&self) -> TextPosition {
        self.position
    }
}
