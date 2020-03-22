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
    multi::{many0_count, many1_count},
    sequence::pair,
    sequence::{delimited, preceded, terminated, tuple},
    IResult, InputIter, InputTake, Needed, Offset, Slice,
};
use nom_locate::{position, LocatedSpan};

use crate::{
    event::XmlVersion,
    reader::data::BufSlice,
    reader::model,
    utils::chars::{is_char, is_decimal, is_hexadecimal, is_name_char, is_name_start_char, is_whitespace_char},
};

#[derive(Debug)]
pub enum ParsedHint {
    StartTag(StartTagHint),
    Reference(ReferenceHint),
    None,
}

#[derive(Debug)]
pub enum StartTagHint {
    RegularTag,
    EmptyElementTag,
}

#[derive(Debug)]
pub enum ReferenceHint {
    Entity(model::Name),
    Char(char),
}

pub type Span<'a> = LocatedSpan<&'a str>;

pub struct Parsed<'a> {
    pub event: model::Event,
    pub hint: ParsedHint,
    pub span: Span<'a>,
}

impl<'a> Parsed<'a> {
    pub fn new(span: Span<'a>, event: model::Event) -> Parsed<'a> {
        Parsed::new_with_hint(span, event, ParsedHint::None)
    }

    pub fn new_with_hint(span: Span<'a>, event: model::Event, hint: ParsedHint) -> Parsed<'a> {
        Parsed { event, hint, span }
    }
}

type NameParts<'a> = (Option<Span<'a>>, Span<'a>);

macro_rules! parsers {
    ($vis:vis $name:ident($input:ident) -> $out:ty = $body:expr; $($rest:tt)*) => {
        $vis fn $name<'a, E: ParseError<Span<'a>>>($input: Span<'a>) -> IResult<Span<'a>, $out, E> {
            $body
        }
        parsers! { $($rest)* }
    };

    ($vis:vis $name:ident($input:ident) -> $out:ty { $($body:tt)* } $($rest:tt)*) => {
        $vis fn $name<'a, E: ParseError<Span<'a>>>($input: Span<'a>) -> IResult<Span<'a>, $out, E> {
            $($body)*
        }
        parsers! { $($rest)* }
    };

    ($vis:vis $name:ident<$lt:lifetime>($input:ident) -> $out:ty = $body:expr; $($rest:tt)*) => {
        $vis fn $name<$lt, E: ParseError<Span<$lt>>>($input: Span<$lt>) -> IResult<Span<$lt>, $out, E> {
            $body
        }
        parsers! { $($rest)* }
    };

    ($vis:vis $name:ident<$lt:lifetime>($input:ident) -> $out:ty { $($body:tt)* } $($rest:tt)*) => {
        $vis fn $name<$lt, E: ParseError<Span<$lt>>>($input: Span<$lt>) -> IResult<Span<$lt>, $out, E> {
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
        parsers! {
            xml_tag_start(i) -> () = ignore(tag("<?xml"))(i);

            xml_tag_end(i) -> () = ignore(tag("?>"))(i);

            version_num(i) -> XmlVersion = map(
                alt((tag("\"1.0\""), tag("'1.0'"), tag("\"1.1\""), tag("'1.1'"))),
                |res: Span| match *res.fragment() {
                    "\"1.0\"" | "'1.0'" => XmlVersion::Version10,
                    "\"1.1\"" | "'1.1'" => XmlVersion::Version11,
                    _ => unreachable!(),
                },
            )(i);

            version_info(i) -> XmlVersion = map(
                tuple((sp, tag("version"), eq, version_num)),
                |(_, _, _, version)| version
            )(i);

            enc_name<'a>(i) -> Span<'a> = recognize(tuple((
                alpha1,
                take_while(|c| is_alphanumeric(c as u8) || "._-".contains(c)),
            )))(i);

            encoding_decl(i) -> BufSlice = map(
                tuple((sp, tag("encoding"), eq, simple_quoted(enc_name))),
                |(_, _, _, enc_name)| BufSlice::from(*enc_name.fragment()),
            )(i);

            sd_val(i) -> bool = map(
                alt((tag("\"yes\""), tag("'yes'"), tag("\"no\""), tag("'no'"))),
                |res: Span| match *res.fragment() {
                    "\"yes\"" | "'yes'" => true,
                    "\"no\"" | "'no'" => false,
                    _ => unreachable!(),
                },
            )(i);

            sd_decl(i) -> bool = map(
                tuple((sp, tag("standalone"), eq, sd_val)),
                |(_, _, _, standalone)| standalone
            )(i);

            xml_tag_content(i) -> (XmlVersion, Option<BufSlice>, Option<bool>) =
               terminated(tuple((version_info, opt(encoding_decl), opt(sd_decl))), opt(sp))(i);

            xml_tag(i) -> model::Event = map(
                surrounded_with_cut(xml_tag_start, xml_tag_content, xml_tag_end),
                |(version, encoding, standalone)| {
                    model::Event::start_document(
                        version,
                        encoding.map(BufSlice::from).unwrap_or(BufSlice::new_static("UTF-8")),
                        standalone,
                    )
                },
            )(i);
        }

        context("XML declaration", parsed(xml_tag))(i)
    }

    pub(super) doctype_declaration(i) -> Parsed {
        parsers! {
            doctype_start(i) -> () = ignore(tag("<!DOCTYPE"))(i);

            doctype_end(i) -> () = ignore(tag(">"))(i);

            doctype_body<'a>(i) -> Span<'a> = recognize_many0(alt((
                take_while1(|c| c != '<' && c != '>'),
                recognize(delimited(tag("<"), doctype_body, tag(">"))),
            )))(i);

            doctype(i) -> model::Event = map(
                surrounded_with_cut(doctype_start, doctype_body, doctype_end),
                |content| model::Event::doctype_declaration(*content.fragment()),
            )(i);
        }

        context("DOCTYPE declaration", parsed(doctype))(i)
    }

    pub(super) outside_tag(i) -> Parsed {
        // The order is important: start_tag would attempt to consume everything starting with `<`, so
        // here we must put other pieces with more complex prefix (e.g. `<?`, `</`, etc) first
        context(
            "Outside tag",
            alt((end_tag, cdata, processing_instruction, comment, start_tag, reference, char_data))
        )(i)
    }

    pub(super) reference(i) -> Parsed {
        parsers! {
            entity_start(i) -> () = ignore(tag("&"))(i);

            entity_end(i) -> () = ignore(tag(";"))(i);

            // TODO: map_res seems to ignore the actual error content

            hexadecimal_reference(i) -> u32 = map_res(
                preceded(tag("x"), cut(recognize_many1(char_matching(is_hexadecimal)))),
                |number_str| u32::from_str_radix(number_str.fragment(), 16)
            )(i);

            decimal_reference(i) -> u32 = map_res(
                recognize_many1(char_matching(is_decimal)),
                |number_str| u32::from_str_radix(number_str.fragment(), 10)
            )(i);

            entity_reference_body(i) -> ReferenceHint = map(name, ReferenceHint::Entity)(i);

            char_reference_body(i) -> ReferenceHint = map(
                verify(
                    preceded(tag("#"), cut(alt((hexadecimal_reference, decimal_reference)))),
                    |n| std::char::from_u32(*n).map(is_char) == Some(true)
                ),
                |n| ReferenceHint::Char(std::char::from_u32(n).unwrap())
            )(i);

            entity_body(i) -> ReferenceHint = alt((char_reference_body, entity_reference_body))(i);

            entity(i) -> ReferenceHint = surrounded_with_cut(entity_start, entity_body, entity_end)(i);

            entity_event(i) -> (model::Event, ParsedHint) = map(
                run_and_recognize(entity),
                |(ref_slice, hint)| (model::Event::text(*ref_slice.fragment()), ParsedHint::Reference(hint))
            )(i);
        }

        context("Reference", parsed_with_hint(entity_event))(i)
    }

    pub(super) start_tag(i) -> Parsed {
        parsers! {
            tag_start(i) -> () = ignore(tag("<"))(i);

            start_tag_end(i) -> () = ignore(tag(">"))(i);

            empty_tag_end(i) -> () = ignore(tag("/>"))(i);

            tag_end(i) -> StartTagHint = alt((
                map(start_tag_end, |_| StartTagHint::RegularTag),
                map(empty_tag_end, |_| StartTagHint::EmptyElementTag),
            ))(i);

            single_quoted_value<'a>(i) -> Span<'a> = surrounded_with_cut(
                tag("'"),
                take_while(|c| c != '<' && c != '&' && c != '\''),
                tag("\"")
            )(i);

            double_quoted_value<'a>(i) -> Span<'a> = surrounded_with_cut(
                tag("\""),
                take_while(|c| c != '<' && c != '&' && c != '"'),
                tag("\"")
            )(i);

            attribute_name_parts<'a>(i) -> NameParts<'a> = context("attribute name", name_parts)(i);

            attribute_value<'a>(i) -> Span<'a> = context(
                "attribute value",
                alt((single_quoted_value, double_quoted_value))
            )(i);

            attribute<'a>(i) -> (NameParts<'a>, Span<'a>) = context(
                "attribute",
                pair(attribute_name_parts, cut(preceded(eq, attribute_value)))
            )(i);

            // Manually unrolled Many0 with custom logic for conflicting attributes
            attributes(i) -> Vec<model::Attribute> {
                let mut i = i;

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

                            let ((opt_prefix, local_name), value) = o;
                            let opt_prefix = opt_prefix.map(|p| *p.fragment());
                            let local_name = *local_name.fragment();

                            if known_names.contains(&(opt_prefix, local_name)) {
                                // TODO: use custom error about duplicate attributes
                                return Err(nom::Err::Error(E::from_error_kind(i, ErrorKind::Many0)));
                            }
                            known_names.insert((opt_prefix, local_name));

                            let name = model::Name::maybe_prefixed(local_name, opt_prefix);
                            let attribute = model::Attribute::new(name, *value.fragment());

                            i = i1;
                            acc.push(attribute);
                        }
                    }
                }
            }

            tag_name_and_attributes(i) -> (model::Name, Vec<model::Attribute>) =
                terminated(pair(name, attributes), opt(sp))(i);

            start_tag(i) -> (model::Event, ParsedHint) = map(
                preceded(tag_start, cut(pair(tag_name_and_attributes, tag_end))),
                |((name, attributes), hint)| {
                    (model::Event::start_element(name, attributes), ParsedHint::StartTag(hint))
                },
            )(i);
        }

        context("Start tag", parsed_with_hint(start_tag))(i)
    }

    pub(super) end_tag(i) -> Parsed {
        parsers! {
            tag_start(i) -> () = ignore(tag("</"))(i);

            tag_end(i) -> () = ignore(tag(">"))(i);

            tag_name(i) -> model::Name = terminated(name, opt(sp))(i);

            end_tag(i) -> model::Event = map(
                surrounded_with_cut(tag_start, tag_name, tag_end),
                |name| model::Event::end_element(name),
            )(i);
        }

        context("End tag", parsed(end_tag))(i)
    }

    pub(super) cdata(i) -> Parsed {
        parsers! {
            cdata_start(i) -> () = ignore(tag("<![CDATA["))(i);

            cdata_end(i) -> () = ignore(tag("]]>"))(i);

            cdata_data<'a>(i) -> Span<'a> = verify(
                take_until("]]>"),
                |s: &Span| s.fragment().chars().all(is_char)
            )(i);

            cdata(i) -> model::Event = map(
                surrounded_with_cut(cdata_start, cdata_data, cdata_end),
                |data| model::Event::cdata(*data.fragment()),
            )(i);
        }

        context("CDATA section", parsed(cdata))(i)
    }

    pub(super) comment(i) -> Parsed {
        parsers! {
            comment_start(i) -> () = ignore(tag("<!--"))(i);

            comment_end(i) -> () = ignore(tag("-->"))(i);

            comment_char(i) -> char = char_matching(|c| c != '-' && is_char(c))(i);

            comment_data<'a>(i) -> Span<'a> = recognize_many0(
                alt((
                    comment_char,
                    preceded(tag("-"), comment_char)
                ))
            )(i);

            comment(i) -> model::Event = map(
                surrounded_with_cut(comment_start, comment_data, comment_end),
                |body| model::Event::comment(*body.fragment()),
            )(i);
        }

        context("Comment", parsed(comment))(i)
    }

    pub(super) processing_instruction(i) -> Parsed {
        fn is_valid_pi_target(s: &str) -> bool {
            match s {
                "xml" | "xmL" | "xMl" | "xML" | "Xml" | "XmL" | "XMl" | "XML" => false,
                _ => true,
            }
        }

        parsers! {
            pi_start(i) -> () = ignore(tag("<?"))(i);

            pi_end(i) -> () = ignore(tag("?>"))(i);

            pi_target<'a>(i) -> Span<'a> = verify(nc_name, |s| is_valid_pi_target(s.fragment()))(i);

            pi_data<'a>(i) -> Span<'a> = verify(take_until("?>"), |s: &Span| s.fragment().chars().all(is_char))(i);

            pi_body<'a>(i) -> (Span<'a>, Option<Span<'a>>) = tuple((pi_target, opt(preceded(sp, pi_data))))(i);

            pi(i) -> model::Event = map(
                surrounded_with_cut(pi_start, pi_body, pi_end),
                |(name, data)| model::Event::processing_instruction(*name.fragment(), data.map(|d| *d.fragment()))
            )(i);
        }

        context("Processing instruction", parsed(pi))(i)
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
            parsed(map(data, |data| if data.fragment().chars().all(is_whitespace_char) {
                model::Event::whitespace(*data.fragment())
            } else {
                model::Event::text(*data.fragment())
            }))
        )(i)
    }

    misc(i) -> Parsed {
        alt((comment, processing_instruction))(i)
    }

    name(i) -> model::Name {
        map(
            name_parts,
            |(prefix, local_part)| model::Name::maybe_prefixed(*local_part.fragment(), prefix.map(|p| *p.fragment())),
        )(i)
    }

    name_parts<'a>(i) -> NameParts<'a> {
        pair(
            // TODO: this one should probably have cut() somehow
            context("Name prefix", opt(terminated(nc_name, char(':')))),
            context("Local name", nc_name),
        )(i)
    }

    nc_name<'a>(i) -> Span<'a> {
        let name_start = char_matching(|c| is_name_start_char(c) && c != ':');
        let name_body = recognize_many0(char_matching(|c| is_name_char(c) && c != ':'));
        recognize(tuple((name_start, name_body)))(i)
    }

    sp<'a>(i) -> Span<'a> {
        recognize_many1(char_matching(is_whitespace_char))(i)
    }

    eq(i) -> () {
        ignore(delimited(opt(sp), tag("="), opt(sp)))(i)
    }

    ch(i) -> char {
        char_matching(is_char)(i)
    }
}

fn simple_quoted<'a, F, O, E>(f: F) -> impl Fn(Span<'a>) -> IResult<Span<'a>, O, E>
where
    F: Fn(Span<'a>) -> IResult<Span<'a>, O, E>,
    E: ParseError<Span<'a>>,
{
    // TODO: use cut
    move |i| alt((delimited(tag("'"), &f, tag("'")), delimited(tag("\""), &f, tag("\""))))(i)
}

fn char_matching<'a, E>(pred: impl Fn(char) -> bool) -> impl Fn(Span<'a>) -> IResult<Span<'a>, char, E>
where
    E: ParseError<Span<'a>>,
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
) -> impl for<'a> Fn(Span<'a>) -> IResult<Span<'a>, (Span<'a>, &'p str), E> {
    assert!(!patterns.is_empty());
    move |input| {
        let ac = AhoCorasick::new_auto_configured(patterns);
        match ac.find(input.fragment()) {
            Some(m) => {
                let s = m.start();
                Ok((input.slice(s..), (input.slice(..s), patterns[m.pattern()])))
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
    move |input| {
        let (i, _) = f(input)?;
        Ok((i, ()))
    }
}

fn with_position<I, O, E>(f: impl Fn(I) -> IResult<I, O, E>) -> impl Fn(I) -> IResult<I, (I, O), E>
where
    I: InputIter + InputTake,
    E: ParseError<I>,
{
    move |input| {
        let (input, pos) = position(input)?;
        Ok((pos, f(input)?))
    }
}

fn parsed<'a, E: ParseError<Span<'a>>>(
    f: impl Fn(Span<'a>) -> IResult<Span<'a>, model::Event, E>,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Parsed<'a>, E> {
    map(with_position(f), |(span, event)| Parsed::new(span, event))
}

fn parsed_with_hint<'a, E: ParseError<Span<'a>>>(
    f: impl Fn(Span<'a>) -> IResult<Span<'a>, (model::Event, ParsedHint), E>,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Parsed<'a>, E> {
    map(with_position(f), |(span, (event, hint))| {
        Parsed::new_with_hint(span, event, hint)
    })
}

fn surrounded_with_cut<I, O1, O2, O3, E, P1, P2, P3>(
    preceding: P1,
    inner: P2,
    following: P3,
) -> impl Fn(I) -> IResult<I, O2, E>
where
    I: Clone + Slice<RangeTo<usize>>,
    P1: Fn(I) -> IResult<I, O1, E>,
    P2: Fn(I) -> IResult<I, O2, E>,
    P3: Fn(I) -> IResult<I, O3, E>,
    E: ParseError<I>,
{
    preceded::<I, O1, O2, E, P1, _>(preceding, cut(terminated::<I, O2, O3, E, P2, P3>(inner, following)))
}
