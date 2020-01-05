use std::borrow::Cow;
use std::io;

use arraydeque::ArrayDeque;
use nom::error::{ParseError, VerboseError};
use nom::{Err, IResult, Needed};

use crate::reader::model::buffer::BufSlice;
use crate::{
    event::{XmlEvent, XmlVersion},
    reader::{
        error::{Error, Result},
        model::{self, buffer::Buffer},
        str_read::StrRead,
    },
    ReaderConfig,
};

// TODO: think of making `buffer` into a dedicated structure which can hold references into it, and provide
//       access to them in a safe way, and automatically determine the size of the buffer kept in memory according
//       to the "live" references, i.e. if there is a reference into the buffer, ensure that the buffer is not
//       cleared up until the point of this reference (and when this reference is dropped, clean the buffer
//       automatically to reduce memory consumption). This is essentially arenas?

pub struct Reader<R: StrRead> {
    source: R,
    config: ReaderConfig,
    logic: ParserLogic,
    buffer: Buffer,
    pos: usize,
    next_events: ArrayDeque<[model::Event; 2]>,
}

impl<R: StrRead> Reader<R> {
    pub fn new(config: ReaderConfig, source: R) -> Reader<R> {
        Reader {
            source,
            config,
            logic: ParserLogic::new(),
            buffer: Buffer::new(),
            pos: 0,
            next_events: ArrayDeque::new(),
        }
    }

    pub fn next(&mut self) -> Result<XmlEvent> {
        if let Some(event) = self.next_events.pop_front() {
            return Ok(event.as_reified(&self.buffer));
        }

        // TODO: Once in a while, we must clear the internal buffer for it not to grow indefinitely.
        //       To do this, we should go through all our in-memory structures pointing to the buffer (e.g. namespaces,
        //       the stack of element names, etc), reify them in-place and then clear the buffer.

        let buffer = &mut *self.buffer as *mut _;

        let event = loop {
            let slice = &self.buffer[self.pos..];
            match self.logic.try_next::<VerboseError<_>>(&self.buffer, slice) {
                Ok(parsed) => {
                    self.pos += parsed.bytes_read;
                    self.next_events.extend_back(parsed.into_iter());

                    break self
                        .next_events
                        .pop_front()
                        .expect("Implementation error: ParserLogic::try_next() returned empty output");
                }

                // TODO: limit reading more data? Otherwise memory overflow is possible for
                //       large documents
                Err(ParserLogicError::Incomplete(_)) => {
                    if !self.source.read_str_data(unsafe { &mut *buffer })? {
                        if self.logic.encountered_element {
                            return Ok(XmlEvent::end_document());
                        }
                        return Err(Error::from(io::Error::new(
                            io::ErrorKind::UnexpectedEof,
                            "Unexpected end of document",
                        )));
                    }
                }

                Err(ParserLogicError::Parsing(e)) => return Err(Error::from((slice, e))),
                Err(ParserLogicError::Logic(e)) => return Err(Error::from(e.into_owned())),
            }
        };

        Ok(event.as_reified(&self.buffer))
    }
}

enum State {
    Prolog(PrologSubstate),
    OutsideTag,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum PrologSubstate {
    BeforeDeclaration,
    BeforeDoctype,
    BeforeDocument,
}

enum ParsedHint {
    StartTag(StartTagHint),
    None,
}

enum StartTagHint {
    RegularTag,
    EmptyElementTag,
}

struct Parsed {
    event: model::Event,
    hint: ParsedHint,
}

impl From<model::Event> for Parsed {
    fn from(event: model::Event) -> Parsed {
        Parsed {
            event,
            hint: ParsedHint::None,
        }
    }
}

trait WithParsedHint {
    fn with_hint(self, hint: ParsedHint) -> Parsed;
}

impl WithParsedHint for model::Event {
    fn with_hint(self, hint: ParsedHint) -> Parsed {
        Parsed { event: self, hint }
    }
}

type PResult<'buf, E> = IResult<&'buf str, Parsed, E>;

struct ParserLogicOutput {
    bytes_read: usize,
    events: ArrayDeque<[model::Event; 3]>,
}

impl ParserLogicOutput {
    fn new(bytes_read: usize) -> ParserLogicOutput {
        ParserLogicOutput {
            bytes_read,
            events: ArrayDeque::new(),
        }
    }

    fn front(&self) -> Option<&model::Event> {
        self.events.front()
    }

    fn push_front(&mut self, event: model::Event) {
        self.events
            .push_front(event)
            .expect("Implementation error: too many events are pushed to the parser output");
    }

    fn push_back(&mut self, event: model::Event) {
        self.events
            .push_back(event)
            .expect("Implementation error: too many events are pushed to the parser output");
    }

    fn into_iter(self) -> impl Iterator<Item = model::Event> {
        self.events.into_iter()
    }
}

enum ParserLogicError<E> {
    Incomplete(Needed),
    Parsing(E),
    Logic(Cow<'static, str>),
}

impl<E> From<Err<E>> for ParserLogicError<E> {
    fn from(e: Err<E>) -> Self {
        match e {
            Err::Error(e) | Err::Failure(e) => ParserLogicError::Parsing(e),
            Err::Incomplete(n) => ParserLogicError::Incomplete(n),
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
    state: State,
    encountered_declaration: bool,
    encountered_element: bool,
    open_elements: Vec<model::Name>,
}

impl ParserLogic {
    fn new() -> ParserLogic {
        ParserLogic {
            state: State::Prolog(PrologSubstate::BeforeDeclaration),
            encountered_declaration: false,
            encountered_element: false,
            open_elements: Vec::new(),
        }
    }

    fn try_next<'buf, E>(&mut self, buffer: &'buf Buffer, input: &'buf str) -> ParserLogicResult<E>
    where
        E: ParseError<&'buf str>,
    {
        let result = match self.state {
            State::Prolog(substate) => self.parse_prolog(input, substate),
            State::OutsideTag => self.parse_outside_tag(input),
        };
        let (remainder, Parsed { event, hint }) = result?;

        let mut output = ParserLogicOutput::new(input.len() - remainder.len());
        output.push_front(event);

        match output.front().unwrap() {
            model::Event::StartDocument { .. } => {
                self.encountered_declaration = true;
                self.state = State::Prolog(PrologSubstate::BeforeDoctype);
            }

            model::Event::DoctypeDeclaration { .. } => {
                self.state = State::Prolog(PrologSubstate::BeforeDocument);

                if !self.encountered_declaration {
                    self.encountered_declaration = true;
                    output.push_front(model::Event::start_document(
                        XmlVersion::Version10,
                        BufSlice::new_static("UTF-8"),
                        None,
                    ));
                }
            }

            model::Event::StartElement { name, .. } => {
                match hint {
                    ParsedHint::StartTag(StartTagHint::EmptyElementTag) => {
                        let name = *name;
                        output.push_back(model::Event::end_element(name));
                    }
                    ParsedHint::StartTag(StartTagHint::RegularTag) => {
                        let name = *name;
                        self.open_elements.push(name);
                    }
                    _ => {
                        debug_assert!(false, "Unexpected ParsedHint");
                    }
                }
                self.encountered_element = true;
                self.state = State::OutsideTag;

                if !self.encountered_declaration {
                    self.encountered_declaration = true;
                    output.push_front(model::Event::start_document(
                        XmlVersion::Version10,
                        BufSlice::new_static("UTF-8"),
                        None,
                    ));
                }
            }

            model::Event::EndElement { name } => {
                match self.open_elements.last() {
                    Some(open_name) => {
                        let open_name = open_name.as_reified(buffer);
                        let name = name.as_reified(buffer);
                        if name != open_name {
                            return Err(
                                format!("Unexpected closing element '{}', expected '{}'", name, open_name).into(),
                            );
                        }
                    }
                    None => {
                        return Err("Unexpected closing element, expected an opening element".into());
                    }
                }
                self.state = State::OutsideTag;
            }

            _ => {}
        }

        Ok(output)
    }

    fn parse_prolog<'buf, E>(&mut self, input: &'buf str, substate: PrologSubstate) -> PResult<'buf, E>
    where
        E: ParseError<&'buf str>,
    {
        match substate {
            PrologSubstate::BeforeDeclaration => parsers::before_declaration(input),
            PrologSubstate::BeforeDoctype => parsers::before_doctype(input),
            PrologSubstate::BeforeDocument => parsers::before_document(input),
        }
    }

    fn parse_outside_tag<'buf, E>(&mut self, input: &'buf str) -> PResult<'buf, E>
    where
        E: ParseError<&'buf str>,
    {
        todo!()
    }
}

mod parsers {
    use std::ops::RangeTo;

    use nom::{
        branch::alt,
        bytes::complete::take_while1,
        bytes::streaming::{tag, take_until, take_while},
        character::streaming::anychar,
        character::{
            is_alphanumeric,
            streaming::{alpha1, char},
        },
        combinator::{cut, map, opt, recognize, verify},
        error::context,
        error::ParseError,
        multi::many0,
        multi::{many0_count, many1_count},
        sequence::pair,
        sequence::{delimited, preceded, terminated, tuple},
        IResult, Offset, Slice,
    };

    use super::{PResult, ParsedHint, WithParsedHint};
    use crate::chars::{is_char, is_name_char, is_name_start_char, is_whitespace_char};
    use crate::event::XmlVersion;
    use crate::reader::model;
    use crate::reader::model::buffer::BufSlice;
    use crate::reader::parsing::StartTagHint;

    pub(super) fn before_declaration<'a, E: ParseError<&'a str>>(i: &'a str) -> PResult<'a, E> {
        context("before declaration", alt((xml_declaration, before_doctype)))(i)
    }

    pub(super) fn before_doctype<'a, E: ParseError<&'a str>>(i: &'a str) -> PResult<'a, E> {
        context(
            "before doctype",
            preceded(opt(sp), alt((misc, doctype_declaration, start_tag))),
        )(i)
    }

    pub(super) fn before_document<'a, E: ParseError<&'a str>>(i: &'a str) -> PResult<'a, E> {
        context("before document", preceded(opt(sp), alt((misc, start_tag))))(i)
    }

    pub(super) fn xml_declaration<'a, E: ParseError<&'a str>>(i: &'a str) -> PResult<'a, E> {
        let xml_tag_start = tag("<?xml");

        let version_num = map(
            alt((tag("\"1.0\""), tag("'1.0'"), tag("\"1.1\""), tag("'1.1'"))),
            |res: &str| match res {
                "\"1.0\"" | "'1.0'" => XmlVersion::Version10,
                "\"1.1\"" | "'1.1'" => XmlVersion::Version11,
                _ => unreachable!(),
            },
        );
        let version_info = map(tuple((sp, tag("version"), eq, version_num)), |(_, _, _, version)| {
            version
        });

        let enc_name = recognize(tuple((
            alpha1,
            take_while(|c| is_alphanumeric(c as u8) || "._-".contains(c)),
        )));

        let encoding_decl = map(
            tuple((sp, tag("encoding"), eq, simple_quoted(enc_name))),
            |(_, _, _, enc_name)| BufSlice::from(enc_name),
        );

        let sd_val = map(
            alt((tag("\"yes\""), tag("'yes'"), tag("\"no\""), tag("'no'"))),
            |res: &str| match res {
                "\"yes\"" | "'yes'" => true,
                "\"no\"" | "'no'" => false,
                _ => unreachable!(),
            },
        );

        let sd_decl = map(tuple((sp, tag("standalone"), eq, sd_val)), |(_, _, _, standalone)| {
            standalone
        });

        let xml_tag_content = tuple((version_info, opt(encoding_decl), opt(sd_decl), opt(sp)));

        let xml_tag_end = tag("?>");

        context(
            "XML declaration",
            map(
                preceded(xml_tag_start, cut(terminated(xml_tag_content, xml_tag_end))),
                |(version, encoding, standalone, _)| {
                    model::Event::start_document(
                        version,
                        encoding.map(BufSlice::from).unwrap_or(BufSlice::new_static("UTF-8")),
                        standalone,
                    )
                    .into()
                },
            ),
        )(i)
    }

    pub(super) fn doctype_declaration<'a, E: ParseError<&'a str>>(i: &'a str) -> PResult<'a, E> {
        let doctype_start = context("DOCTYPE start", tag("<!DOCTYPE"));

        fn doctype_body<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
            context(
                "DOCTYPE body",
                recognize_many0(alt((
                    take_while1(|c| c != '<' && c != '>'),
                    recognize(delimited(tag("<"), doctype_body, tag(">"))),
                ))),
            )(i)
        }

        let doctype_end = context("DOCTYPE end", tag(">"));

        context(
            "DOCTYPE declaration",
            map(
                preceded(doctype_start, cut(terminated(doctype_body, doctype_end))),
                |content| model::Event::doctype_declaration(content).into(),
            ),
        )(i)
    }

    pub(super) fn start_tag<'a, E: ParseError<&'a str>>(i: &'a str) -> PResult<'a, E> {
        let tag_start = tag("<");

        fn attribute<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, model::Attribute, E> {
            let single_quoted_value = preceded(
                tag("'"),
                cut(terminated(take_while(|c| c != '<' && c != '&' && c != '\''), tag("'"))),
            );

            let double_quoted_value = preceded(
                tag("\""),
                cut(terminated(take_while(|c| c != '<' && c != '&' && c != '"'), tag("\""))),
            );

            let attribute_name = context("attribute name", name);

            let value = context("attribute value", alt((single_quoted_value, double_quoted_value)));

            context(
                "attribute",
                map(tuple((attribute_name, cut(preceded(eq, value)))), |(name, value)| {
                    model::Attribute::new(name, value)
                }),
            )(i)
        }

        let start_tag_end = tag(">");

        let empty_tag_end = tag("/>");

        let tag_end = alt((
            map(start_tag_end, |_| StartTagHint::RegularTag),
            map(empty_tag_end, |_| StartTagHint::EmptyElementTag),
        ));

        let attributes = many0(preceded(opt(sp), attribute));

        let tag_name_and_attributes = terminated(pair(name, attributes), opt(sp));

        context(
            "start tag",
            map(
                preceded(tag_start, cut(pair(tag_name_and_attributes, tag_end))),
                |((name, attributes), hint)| {
                    model::Event::start_element(name, attributes).with_hint(ParsedHint::StartTag(hint))
                },
            ),
        )(i)
    }

    pub(super) fn comment<'a, E: ParseError<&'a str>>(i: &'a str) -> PResult<'a, E> {
        let comment_start = tag("<!--");

        let comment_char = &|i| char_matching(|c| c != '-' && is_char(c))(i);

        let comment_body = recognize_many0(alt((comment_char, preceded(tag("-"), comment_char))));

        let comment_end = tag("-->");

        context(
            "comment",
            map(
                preceded(comment_start, cut(terminated(comment_body, comment_end))),
                |body| model::Event::comment(body).into(),
            ),
        )(i)
    }

    pub(super) fn processing_instruction<'a, E: ParseError<&'a str>>(i: &'a str) -> PResult<'a, E> {
        let pi_start = tag("<?");

        let pi_end = tag("?>");

        fn is_valid_pi_target(s: &str) -> bool {
            match s {
                "xml" | "xmL" | "xMl" | "xML" | "Xml" | "XmL" | "XMl" | "XML" => false,
                _ => true,
            }
        }

        let pi_target = verify(nc_name, |s: &&str| is_valid_pi_target(*s));

        let pi_data = verify(take_until("?>"), |s: &str| s.chars().all(is_char));

        let pi_body = tuple((pi_target, opt(preceded(sp, pi_data))));

        context(
            "processing instruction",
            map(preceded(pi_start, cut(terminated(pi_body, pi_end))), |(name, data)| {
                model::Event::processing_instruction(name, data).into()
            }),
        )(i)
    }

    fn misc<'a, E: ParseError<&'a str>>(i: &'a str) -> PResult<'a, E> {
        alt((comment, processing_instruction))(i)
    }

    fn name<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, model::Name, E> {
        map(
            pair(
                // TODO: this one should probably have cut() somehow
                context("name prefix", opt(terminated(nc_name, char(':')))),
                context("local name", nc_name),
            ),
            |(prefix, local_part)| model::Name::maybe_prefixed(local_part, prefix),
        )(i)
    }

    fn nc_name<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
        let name_start = char_matching(|c| is_name_start_char(c) && c != ':');
        let name_body = recognize_many0(char_matching(|c| is_name_char(c) && c != ':'));
        recognize(tuple((name_start, name_body)))(i)
    }

    fn sp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
        recognize_many1(char_matching(is_whitespace_char))(i)
    }

    fn eq<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
        ignore(delimited(opt(sp), tag("="), opt(sp)))(i)
    }

    fn ch<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, char, E> {
        char_matching(is_char)(i)
    }

    fn simple_quoted<'a, F, O, E: ParseError<&'a str>>(f: F) -> impl Fn(&'a str) -> IResult<&'a str, O, E>
    where
        F: Fn(&'a str) -> IResult<&'a str, O, E>,
    {
        // TODO: use cut
        move |i| alt((delimited(tag("'"), &f, tag("'")), delimited(tag("\""), &f, tag("\""))))(i)
    }

    fn char_matching<'a, E>(pred: impl Fn(char) -> bool) -> impl Fn(&'a str) -> IResult<&'a str, char, E>
    where
        E: ParseError<&'a str>,
    {
        verify(anychar, move |&c| pred(c))
    }

    fn recognize_many1<I, O, E>(f: impl Fn(I) -> IResult<I, O, E>) -> impl Fn(I) -> IResult<I, I, E>
    where
        I: Clone + Offset + Slice<RangeTo<usize>> + PartialEq,
        E: ParseError<I>,
    {
        recognize(many1_count(f))
    }

    fn recognize_many0<I, O, E>(f: impl Fn(I) -> IResult<I, O, E>) -> impl Fn(I) -> IResult<I, I, E>
    where
        I: Clone + Offset + Slice<RangeTo<usize>> + PartialEq,
        E: ParseError<I>,
    {
        recognize(many0_count(f))
    }

    fn ignore<I, O, E: ParseError<I>>(f: impl Fn(I) -> IResult<I, O, E>) -> impl Fn(I) -> IResult<I, (), E> {
        map(f, |_| ())
    }
}
