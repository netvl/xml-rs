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
    IResult, Needed, Offset, Slice,
};

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

pub struct Parsed {
    pub event: model::Event,
    pub hint: ParsedHint,
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
            |n| ReferenceHint::Char(std::char::from_u32(n).unwrap())
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
            map(data, |data| if data.chars().all(is_whitespace_char) {
                model::Event::whitespace(data).into()
            } else {
                model::Event::text(data).into()
            })
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
