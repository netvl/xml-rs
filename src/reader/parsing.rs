use std::borrow::Cow;
use std::collections::VecDeque;
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
    next_events: VecDeque<model::CowEvent>,
    encountered_end_or_error: bool,
}

impl<R: StrRead> Reader<R> {
    pub fn new(config: ReaderConfig, source: R) -> Reader<R> {
        Reader {
            source,
            config,
            logic: ParserLogic::new(),
            buffer: Buffer::new(),
            pos: 0,
            next_events: VecDeque::new(),
            encountered_end_or_error: false,
        }
    }

    pub fn next(&mut self) -> Result<XmlEvent> {
        if let Some(event) = self.next_events.pop_front() {
            return Ok(event.reify(&self.buffer));
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
                    self.next_events.extend(parsed.into_iter().map(model::CowEvent::from));

                    if self.should_continue_reading() {
                        continue;
                    }

                    break self.compute_next_event();
                }

                // TODO: limit reading more data? Otherwise memory overflow is possible for
                //       large documents
                Err(ParserLogicError::Incomplete(_)) => {
                    let buffer = unsafe { &mut *buffer };

                    if self.source.need_to_grow(buffer)? {
                        for event in &mut self.next_events {
                            event.reify_in_place(&self.buffer);
                        }
                        self.logic.reify_state(&self.buffer);
                    }

                    if !self.source.read_str_data(buffer)? {
                        if self.logic.encountered_element && self.logic.open_elements.is_empty() {
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

        Ok(event.reify(&self.buffer))
    }

    pub fn fused_next(&mut self) -> Option<Result<XmlEvent>> {
        let encountered_end_or_error = unsafe { &mut *(&mut self.encountered_end_or_error as *mut _) };
        if *encountered_end_or_error {
            None
        } else {
            let event = self.next();
            match event {
                Ok(XmlEvent::EndDocument) | Err(_) => {
                    *encountered_end_or_error = true;
                }
                _ => {}
            }
            Some(event)
        }
    }

    fn should_continue_reading(&self) -> bool {
        // We must analyze the tail of the self.next_events queue here
        // If for example we have only a textual event here, and nothing else, we should continue reading
        // and then potentially merge the adjacent textual events
        false
    }

    fn compute_next_event(&mut self) -> model::CowEvent {
        self.next_events
            .pop_front()
            .expect("Implementation error: ParserLogic::try_next() returned empty output")
    }
}

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
enum ParsedHint {
    StartTag(StartTagHint),
    Reference(ReferenceHint),
    None,
}

#[derive(Debug)]
enum StartTagHint {
    RegularTag,
    EmptyElementTag,
}

#[derive(Debug)]
enum ReferenceHint {
    Entity(model::Name),
    Char(u32),
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

#[derive(Debug)]
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
    open_elements: Vec<model::CowName>,
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

    fn reify_state(&mut self, buffer: &Buffer) {
        for name in &mut self.open_elements {
            name.reify_in_place(buffer);
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
                        self.open_elements.push(name.into());
                    }
                    hint => {
                        debug_assert!(false, "Unexpected ParsedHint: {:?}", hint);
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

            model::Event::EndElement { name } => match self.open_elements.pop() {
                Some(open_name) => {
                    let open_name = open_name.as_reified(buffer);
                    let name = name.as_reified(buffer);
                    if name != open_name {
                        return Err(format!("Unexpected closing element '{}', expected '{}'", name, open_name).into());
                    }
                }
                None => {
                    return Err("Unexpected closing element, expected an opening element".into());
                }
            },

            model::Event::Comment(_) => {
                // A comment in the header means that no declaration must follow
                self.state = match self.state {
                    State::Prolog(PrologSubstate::BeforeDeclaration) => State::Prolog(PrologSubstate::BeforeDoctype),
                    other => other,
                }
            }

            model::Event::ProcessingInstruction { .. } => {
                // A PI in the header means that no declaration must follow
                self.state = match self.state {
                    State::Prolog(PrologSubstate::BeforeDeclaration) => State::Prolog(PrologSubstate::BeforeDoctype),
                    other => other,
                }
            }

            model::Event::CData(_) => {
                // nothing to do
            }

            model::Event::Text(_) => {
                match hint {
                    ParsedHint::Reference(ReferenceHint::Char(char_code)) => {
                        // TODO: add char_code to output and proceed with parsing further
                    }
                    ParsedHint::Reference(ReferenceHint::Entity(name)) => {
                        // TODO: resolve the name and push the result to the stack or throw an error
                    }
                    ParsedHint::None => {
                        // Just regular text
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

    fn parse_prolog<'buf, E>(&mut self, input: &'buf str, substate: PrologSubstate) -> IResult<&'buf str, Parsed, E>
    where
        E: ParseError<&'buf str>,
    {
        match substate {
            PrologSubstate::BeforeDeclaration => parsers::before_declaration(input),
            PrologSubstate::BeforeDoctype => parsers::before_doctype(input),
            PrologSubstate::BeforeDocument => parsers::before_document(input),
        }
    }

    fn parse_outside_tag<'buf, E>(&mut self, input: &'buf str) -> IResult<&'buf str, Parsed, E>
    where
        E: ParseError<&'buf str>,
    {
        parsers::outside_tag(input)
    }
}

mod parsers {
    use std::collections::HashSet;
    use std::ops::RangeTo;

    use aho_corasick::AhoCorasick;
    use nom::{
        branch::alt,
        bytes::complete::take_while1,
        bytes::streaming::{tag, take_until, take_while},
        character::streaming::anychar,
        character::{
            is_alphanumeric,
            streaming::{alpha1, char},
        },
        combinator::{cut, map, map_res, opt, recognize, verify},
        error::context,
        error::ErrorKind,
        error::ParseError,
        multi::many0,
        multi::{many0_count, many1_count},
        sequence::pair,
        sequence::{delimited, preceded, terminated, tuple},
        IResult, Needed, Offset, Slice,
    };

    use super::{Parsed, ParsedHint, ReferenceHint, WithParsedHint};
    use crate::chars::{is_char, is_decimal, is_hexadecimal, is_name_char, is_name_start_char, is_whitespace_char};
    use crate::event::XmlVersion;
    use crate::reader::model;
    use crate::reader::model::buffer::BufSlice;
    use crate::reader::parsing::StartTagHint;

    macro_rules! parsers {
        ($vis:vis $name:ident($input:ident) -> $out:ty = $body:expr; $($rest:tt)*) => {
            $vis fn $name<'a, E: ParseError<&'a str>>($input: &'a str) -> IResult<&'a str, $out, E> {
                $body
            }
            parsers! { $($rest)* }
        };

        ($vis:vis $name:ident($input:ident) -> $out:ty { $($body:tt)* } $($rest:tt)*) => {
            $vis fn $name<'a, E: ParseError<&'a str>>($input: &'a str) -> IResult<&'a str, $out, E> {
                $($body)*
            }
            parsers! { $($rest)* }
        };

        ($vis:vis $name:ident<$lt:lifetime>($input:ident) -> $out:ty = $body:expr; $($rest:tt)*) => {
            $vis fn $name<$lt, E: ParseError<&$lt str>>($input: &$lt str) -> IResult<&$lt str, $out, E> {
                $body
            }
            parsers! { $($rest)* }
        };

        ($vis:vis $name:ident<$lt:lifetime>($input:ident) -> $out:ty { $($body:tt)* } $($rest:tt)*) => {
            $vis fn $name<$lt, E: ParseError<&$lt str>>($input: &$lt str) -> IResult<&$lt str, $out, E> {
                $($body)*
            }
            parsers! { $($rest)* }
        };

        () => {}
    }

    parsers! {
        pub(super) before_declaration(i) -> Parsed {
            context("Before declaration", alt((xml_declaration, before_doctype)))(i)
        }

        pub(super) before_doctype(i) -> Parsed {
            context(
                "Before doctype",
                preceded(opt(sp), alt((misc, doctype_declaration, start_tag))),
            )(i)
        }

        pub(super) before_document(i) -> Parsed {
            context("Before document", preceded(opt(sp), alt((misc, start_tag))))(i)
        }

        pub(super) xml_declaration(i) -> Parsed {
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

        pub(super) doctype_declaration(i) -> Parsed {
            let doctype_start = context("DOCTYPE start", tag("<!DOCTYPE"));

            parsers! {
                doctype_body<'a>(i) -> &'a str {
                    context(
                        "DOCTYPE body",
                        recognize_many0(alt((
                            take_while1(|c| c != '<' && c != '>'),
                            recognize(delimited(tag("<"), doctype_body, tag(">"))),
                        ))),
                    )(i)
                }
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

        pub(super) outside_tag(i) -> Parsed {
            // The order is important: start_tag would attemt to consume everything starting with `<`, so
            // here we must put other pieces with more complex prefix (e.g. `<?`, `</`, etc) first
            context(
                "Outside tag",
                alt((end_tag, cdata, processing_instruction, comment, start_tag, reference, char_data))
            )(i)
        }

        pub(super) reference(i) -> Parsed {
            parsers! {
                entity_start<'a>(i) -> &'a str = tag("&")(i);
                entity_end<'a>(i) -> &'a str = tag(";")(i);

                // TODO: map_res seems to ignore the actual error content

                hexadecimal_reference(i) -> u32 {
                    map_res(
                        preceded(tag("x"), cut(recognize_many1(char_matching(is_hexadecimal)))),
                        |number_str| u32::from_str_radix(number_str, 16)
                    )(i)
                }

                decimal_reference(i) -> u32 {
                    map_res(
                        recognize_many1(char_matching(is_decimal)),
                        |number_str| u32::from_str_radix(number_str, 10)
                    )(i)
                }
            }

            let entity_reference_body = map(
                name,
                |name| ReferenceHint::Entity(name)
            );

            let char_reference_body = map(
                verify(
                    preceded(tag("#"), cut(alt((hexadecimal_reference, decimal_reference)))),
                    |n| std::char::from_u32(*n).map(is_char) == Some(true)
                ),
                |n| ReferenceHint::Char(n)
            );

            let entity_body = alt((char_reference_body, entity_reference_body));

            let entity = preceded(entity_start, cut(terminated(entity_body, entity_end)));

            context(
                "Reference",
                map(
                    run_and_recognize(entity),
                    |(ref_slice, hint)| model::Event::text(ref_slice).with_hint(ParsedHint::Reference(hint))
                )
            )(i)
        }

        pub(super) start_tag(i) -> Parsed {
            let tag_start = tag("<");

            fn attribute<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, ((Option<&'a str>, &'a str), &'a str), E> {
                let single_quoted_value = preceded(
                    tag("'"),
                    cut(terminated(take_while(|c| c != '<' && c != '&' && c != '\''), tag("'"))),
                );

                let double_quoted_value = preceded(
                    tag("\""),
                    cut(terminated(take_while(|c| c != '<' && c != '&' && c != '"'), tag("\""))),
                );

                let attribute_name_parts = context("attribute name", name_parts);

                let value = context("attribute value", alt((single_quoted_value, double_quoted_value)));

                context(
                    "Attribute",
                    pair(attribute_name_parts, cut(preceded(eq, value)))
                )(i)
            }

            let start_tag_end = tag(">");

            let empty_tag_end = tag("/>");

            let tag_end = alt((
                map(start_tag_end, |_| StartTagHint::RegularTag),
                map(empty_tag_end, |_| StartTagHint::EmptyElementTag),
            ));


            fn attributes<'a, E: ParseError<&'a str>>(mut i: &'a str) -> IResult<&'a str, Vec<model::Attribute>, E> {
                let single_attribute = preceded(sp, attribute);

                let mut known_names = HashSet::new();
                let mut acc = Vec::new();
                loop {
                    match single_attribute(i) {
                        Err(nom::Err::Error(_)) => return Ok((i, acc)),
                        Err(e) => return Err(e),
                        Ok((i1, o)) => {
                            if i1 == i {
                              return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Many0)));
                            }

                            let (name_parts, value) = o;
                            if known_names.contains(&name_parts) {
                                return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Many0)));
                            }
                            known_names.insert(name_parts);

                            let name = model::Name::maybe_prefixed(name_parts.1, name_parts.0);
                            let attribute = model::Attribute::new(name, value);

                            i = i1;
                            acc.push(attribute);
                        }
                    }
                }
            }

            let tag_name_and_attributes = terminated(pair(name, attributes), opt(sp));

            context(
                "Start tag",
                map(
                    preceded(tag_start, cut(pair(tag_name_and_attributes, tag_end))),
                    |((name, attributes), hint)| {
                        model::Event::start_element(name, attributes).with_hint(ParsedHint::StartTag(hint))
                    },
                )
            )(i)
        }

        pub(super) end_tag(i) -> Parsed {
            let tag_start = tag("</");

            let tag_end = tag(">");

            let tag_name = terminated(name, opt(sp));

            context(
                "End tag",
                map(
                    preceded(tag_start, cut(terminated(tag_name, tag_end))),
                    |name| model::Event::end_element(name).into()
                )
            )(i)
        }

        pub(super) cdata(i) -> Parsed {
            let cdata_start = tag("<![CDATA[");

            let cdata_data = verify(take_until("]]>"), |s: &str| s.chars().all(is_char));

            let cdata_end = tag("]]>");

            context(
                "CDATA section",
                map(
                    preceded(cdata_start, cut(terminated(cdata_data, cdata_end))),
                    |data| model::Event::cdata(data).into()
                )
            )(i)
        }

        pub(super) comment(i) -> Parsed {
            let comment_start = tag("<!--");

            let comment_char = &|i| char_matching(|c| c != '-' && is_char(c))(i);

            let comment_body = recognize_many0(alt((comment_char, preceded(tag("-"), comment_char))));

            let comment_end = tag("-->");

            context(
                "Comment",
                map(
                    preceded(comment_start, cut(terminated(comment_body, comment_end))),
                    |body| model::Event::comment(body).into(),
                )
            )(i)
        }

        pub(super) processing_instruction(i) -> Parsed {
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
                "Processing instruction",
                map(
                    preceded(pi_start, cut(terminated(pi_body, pi_end))),
                    |(name, data)| model::Event::processing_instruction(name, data).into()
                )
            )(i)
        }

        char_data(i) -> Parsed {
            // TODO: maybe replace with verify?
            let data = map_res(
                take_until_matching(&["<", "&", "]]>"]),
                |(result, pattern)| match pattern {
                    "]]>" => Err(()),  // error does not matter, it is ignored
                    _ => Ok(result),
                }
            );

            context(
                "Character data",
                map(data, |data| model::Event::text(data).into())
            )(i)
        }

        misc(i) -> Parsed {
            alt((comment, processing_instruction))(i)
        }

        name_parts<'a>(i) -> (Option<&'a str>, &'a str) {
            pair(
                // TODO: this one should probably have cut() somehow
                context("Name prefix", opt(terminated(nc_name, char(':')))),
                context("Local name", nc_name),
            )(i)
        }

        name(i) -> model::Name {
            map(
                name_parts,
                |(prefix, local_part)| model::Name::maybe_prefixed(local_part, prefix),
            )(i)
        }

        nc_name<'a>(i) -> &'a str {
            let name_start = char_matching(|c| is_name_start_char(c) && c != ':');
            let name_body = recognize_many0(char_matching(|c| is_name_char(c) && c != ':'));
            recognize(tuple((name_start, name_body)))(i)
        }

        sp<'a>(i) -> &'a str {
            recognize_many1(char_matching(is_whitespace_char))(i)
        }

        eq(i) -> () {
            ignore(delimited(opt(sp), tag("="), opt(sp)))(i)
        }

        ch(i) -> char {
            char_matching(is_char)(i)
        }
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

    fn run_and_recognize<I, O, E>(p: impl Fn(I) -> IResult<I, O, E>) -> impl Fn(I) -> IResult<I, (I, O), E>
    where
        I: Clone + Offset + Slice<RangeTo<usize>>,
        E: ParseError<I>,
    {
        move |input: I| {
            let i = input.clone();
            match p(i) {
                Ok((i, o)) => {
                    let index = input.offset(&i);
                    Ok((i, (input.slice(..index), o)))
                }
                Err(e) => Err(e),
            }
        }
    }

    fn take_until_matching<'p, E>(
        patterns: &'p [&'p str],
    ) -> impl for<'a> Fn(&'a str) -> IResult<&'a str, (&'a str, &'p str), E> {
        assert!(!patterns.is_empty());
        move |input| {
            let ac = AhoCorasick::new_auto_configured(patterns);
            match ac.find(input) {
                Some(m) => {
                    let s = m.start();
                    Ok((&input[s..], (&input[..s], patterns[m.pattern()])))
                }
                None => {
                    // Need at least the length of the smallest pattern to continue
                    let required = patterns.iter().cloned().map(str::len).min().unwrap();
                    Err(nom::Err::Incomplete(Needed::Size(required)))
                }
            }
        }
    }

    fn ignore<I, O, E: ParseError<I>>(f: impl Fn(I) -> IResult<I, O, E>) -> impl Fn(I) -> IResult<I, (), E> {
        map(f, |_| ())
    }
}
