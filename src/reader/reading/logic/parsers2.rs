use combine::parser::range::take_while;
use combine::{
    attempt, choice, one_of, opaque, optional,
    parser::combinator::{ignore, no_partial, FnOpaque},
    parser::range::range,
    parser::range::{recognize, take_while1},
    position, satisfy, skip_many, skip_many1,
    stream::position::SourcePosition,
    stream::Range,
    token, ParseError, Parser, RangeStream, Stream,
};

use crate::event::XmlVersion;
use crate::reader::data::BufSlice;
use crate::reader::model;
use crate::utils::chars::{is_char, is_whitespace_char};
use crate::utils::position::TextPosition;

#[derive(Debug, Copy, Clone)]
pub enum ParsedHint {
    StartTag(StartTagHint),
    Reference(ReferenceHint),
    None,
}

#[derive(Debug, Copy, Clone)]
pub enum StartTagHint {
    RegularTag,
    EmptyElementTag,
}

#[derive(Debug, Copy, Clone)]
pub enum ReferenceHint {
    Entity(model::Name),
    Char(char),
}

#[derive(Debug, Clone)]
pub struct ParsedEvent {
    pub event: model::Event,
    pub hint: ParsedHint,
    pub position: TextPosition,
}

impl ParsedEvent {
    fn new(event: model::Event, position: TextPosition) -> ParsedEvent {
        ParsedEvent {
            event,
            position: position.into(),
            hint: ParsedHint::None,
        }
    }
}

impl From<SourcePosition> for TextPosition {
    fn from(pos: SourcePosition) -> Self {
        TextPosition {
            row: (pos.line as u64).saturating_sub(1),
            column: (pos.column as u64).saturating_sub(1),
        }
    }
}

pub fn xml_declaration<'buf, I: 'buf>() -> impl Parser<I, Output = ParsedEvent> + 'buf
where
    I: RangeStream<Token = char, Range = &'buf str, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    // version = "x.y"

    let version_num_inner = || {
        (range("1."), one_of("01".chars())).map(|(_, c)| match c {
            '0' => XmlVersion::Version10,
            '1' => XmlVersion::Version11,
            _ => unreachable!(),
        })
    };
    let version_num = quoted(version_num_inner);

    let version_info = (range("version"), eq(), version_num).map(|(_, _, version)| version);

    // encoding = "XYZ"

    fn enc_name_pred(c: char) -> bool {
        c.is_ascii_alphanumeric() || "._-".contains(c)
    }
    let enc_name_inner = || recognize((ascii_alpha(), take_while(enc_name_pred)));
    let enc_name = || quoted(enc_name_inner);

    let encoding_decl = || (range("encoding"), eq(), enc_name()).map(|(_, _, enc_name)| BufSlice::from(enc_name));

    // standalone = "yes/no"

    let sd_val_inner = || {
        range("yes").or(range("no")).map(|s| match s {
            "yes" => true,
            "no" => false,
            _ => unreachable!(),
        })
    };
    let sd_val = || quoted(sd_val_inner);

    let sd_decl = || (range("standalone"), eq(), sd_val()).map(|(_, _, standalone)| standalone);

    // encoding = "XYZ" followed by optional standalone = "yes/no"

    let encoding_then_sd_decl = (encoding_decl(), optional(sp1().with(sd_decl()))).map(|(e, opt_sd)| (Some(e), opt_sd));

    // either (encoding = "XYZ" followed by optional standalone = "yes/no") or (standalone = "yes/no")

    let encoding_sd_decl_variants = encoding_then_sd_decl.or(sd_decl().map(|sd| (None, Some(sd))));

    // optional (space + either (encoding = "XYZ" followed by optional standalone = "yes/no") or (standalone = "yes/no"))

    let opt_encoding_sd_decl_variants = optional(sp1().with(encoding_sd_decl_variants)).map(|r| match r {
        None => (None, None),
        Some((encoding, sd)) => (encoding, sd),
    });

    // Between '<?xml' and '?>'

    let xml_tag_content = sp1()
        .with((version_info, opt_encoding_sd_decl_variants))
        .skip(sp())
        .map(|(version, (encoding, standalone))| (version, encoding, standalone));

    // Whole '<?xml ... ?>'

    let xml_tag_start = range("<?xml");
    let xml_tag_end = range("?>");

    let xml_tag = delimited(xml_tag_start, xml_tag_content, xml_tag_end).map(|(version, encoding, standalone)| {
        model::Event::start_document(
            version,
            encoding.map(BufSlice::from).unwrap_or(BufSlice::new_static("UTF-8")),
            standalone,
        )
    });

    parsed(xml_tag).message("In XML declaration")
}

pub fn doctype_declaration<'buf, I: 'buf>() -> impl Parser<I, Output = ParsedEvent> + 'buf
where
    I: RangeStream<Token = char, Range = &'buf str, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    let doctype_start = range("<!DOCTYPE").skip(sp1());
    let doctype_end = range(">");

    fn doctype_body<'buf, I>() -> FnOpaque<I, I::Range>
    where
        I: RangeStream<Token = char, Range = &'buf str>,
        I::Error: ParseError<I::Token, I::Range, I::Position>,
    {
        opaque!(no_partial(recognize_many(choice((
            take_while1(|c| c != '<' && c != '>'),
            recognize(delimited(range("<"), doctype_body(), range(">"))),
        )))))
    }

    let doctype =
        delimited(doctype_start, doctype_body(), doctype_end).map(|content| model::Event::doctype_declaration(content));

    parsed(doctype).message("In DOCTYPE declaration")
}

pub fn comment<'buf, I: 'buf>() -> impl Parser<I, Output = ParsedEvent> + 'buf
where
    I: RangeStream<Token = char, Range = &'buf str, Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    let comment_start = range("<!--");
    let comment_end = range("-->");

    let comment_char = || satisfy(|c: char| c != '-' && is_char(c));
    let comment_data = recognize_many(choice((
        comment_char(),
        // attempt is needed, otherwise this one will eagerly parse the first '-' in the '-->' portion of a comment
        // and subsequently fail on the second '-'
        attempt(range("-").with(comment_char())),
    )));

    let comment = delimited(comment_start, comment_data, comment_end).map(|content| model::Event::comment(content));

    parsed(comment).message("In comment")
}

pub fn delimited<I, P1, P2, P3>(first: P1, inner: P2, last: P3) -> impl Parser<I, Output = P2::Output>
where
    I: Stream,
    P1: Parser<I>,
    P2: Parser<I>,
    P3: Parser<I>,
{
    first.with(inner.skip(last))
}

pub fn recognize_many<'buf, I>(inner: impl Parser<I>) -> impl Parser<I, Output = I::Range>
where
    I: RangeStream,
    I::Range: Range,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    recognize(skip_many(inner))
}

pub fn recognize_many1<'buf, I>(inner: impl Parser<I>) -> impl Parser<I, Output = I::Range>
where
    I: RangeStream,
    I::Range: Range,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    recognize(skip_many1(inner))
}

fn parsed<I>(p: impl Parser<I, Output = model::Event>) -> impl Parser<I, Output = ParsedEvent>
where
    I: Stream<Position = SourcePosition>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    (p, position()).map(|(e, pos)| ParsedEvent::new(e, pos.into()))
}

fn quoted<I, P, PP>(p: PP) -> impl Parser<I, Output = P::Output>
where
    P: Parser<I>,
    PP: Fn() -> P,
    I: Stream<Token = char>,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    choice((
        delimited(token('\''), p(), token('\'')),
        delimited(token('"'), p(), token('"')),
    ))
}

fn sp<I>() -> impl Parser<I, Output = I::Range>
where
    I: RangeStream<Token = char>,
    I::Range: Range,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    recognize_many(satisfy(is_whitespace_char))
}

fn sp1<I>() -> impl Parser<I, Output = I::Range>
where
    I: RangeStream<Token = char>,
    I::Range: Range,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    recognize_many1(satisfy(is_whitespace_char))
}

fn eq<I>() -> impl Parser<I, Output = ()>
where
    I: RangeStream<Token = char>,
    I::Range: Range,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    ignore(delimited(sp(), token('='), sp()))
}

fn xml_whitespace<I>() -> impl Parser<I, Output = I::Token>
where
    I: Stream<Token = char>,
    I::Range: Range,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    satisfy(is_whitespace_char).expected("space, carriage return, line feed or tab")
}

fn ascii_alpha<I>() -> impl Parser<I, Output = I::Token>
where
    I: Stream<Token = char>,
    I::Range: Range,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    satisfy(|c: char| c.is_ascii_alphabetic()).expected("ASCII letter")
}

fn ascii_alphanum<I>() -> impl Parser<I, Output = I::Token>
where
    I: Stream<Token = char>,
    I::Range: Range,
    I::Error: ParseError<I::Token, I::Range, I::Position>,
{
    satisfy(|c: char| c.is_ascii_alphanumeric()).expected("ASCII letter or digit")
}

#[cfg(test)]
mod tests {
    use combine::stream::easy::{Error, Info};
    use combine::stream::position::Stream as PositionStream;
    use combine::EasyParser;

    use assert_matches::assert_matches;

    use crate::event::Event;
    use crate::reader::data::Buffer;

    use super::*;

    macro_rules! check_parsed {
        ($result:expr; $buf:expr; $event:expr, $hint:pat, ($row:expr, $col:expr)) => {{
            let (parsed, remaining) = $result;
            assert!(remaining.input.is_empty());
            assert_eq!(parsed.event.as_reified(&$buf), $event);
            assert_matches!(parsed.hint, $hint);
            assert_eq!(parsed.position, TextPosition::new($row as u64, $col as u64));
        }};
        ($result:expr; $buf:expr; $event:expr, $hint:pat) => {
            check_parsed!($result; $buf; $event, $hint, (0, $buf.len()))
        };
    }

    macro_rules! check_parse_error {
        ($result:expr; ($row:expr, $col:expr), $($err:pat),+) => {{
            let result = $result;
            assert_eq!(TextPosition::from(result.position), TextPosition::new($row as u64, $col as u64));
            let mut errors = result.errors.into_iter().peekable();
            $(
                let error = errors.next().expect("Expected additional error");
                assert_matches!(error, $err);
            )+
            if errors.peek().is_some() {
                let errors: Vec<_> = errors.map(|e| format!(" * {:?}", e)).collect();
                panic!("Unexpected additional errors:\n{}", errors.join("\n"));
            }
            assert!(errors.next().is_none(), "Unexpected additional errors");
        }};
    }

    #[test]
    fn test_xml_declaration_full() {
        let data = Buffer::from_str("<?xml version='1.1' encoding = \"UTF-16\" standalone= \"yes\" ?>");
        let result = xml_declaration().easy_parse(PositionStream::new(data.as_str()));
        check_parsed!(result.unwrap(); data; Event::start_document(XmlVersion::Version11, "UTF-16", Some(true)), ParsedHint::None);
    }

    #[test]
    fn test_doctype_declaration_simple() {
        let data = Buffer::from_str("<!DOCTYPE data>");
        let result = doctype_declaration().easy_parse(PositionStream::new(data.as_str()));
        check_parsed!(result.unwrap(); data; Event::doctype_declaration("data"), ParsedHint::None);
    }

    #[test]
    fn test_doctype_nested_angle_brackets() {
        let data = Buffer::from_str("<!DOCTYPE hello<world<nested > whatever>xyz>");
        let result = doctype_declaration().easy_parse(PositionStream::new(data.as_str()));
        check_parsed!(
            result.unwrap(); data;
            Event::doctype_declaration("hello<world<nested > whatever>xyz"),
            ParsedHint::None
        );
    }

    #[test]
    fn test_doctype_wrong_nesting() {
        let data = Buffer::from_str("<!DOCTYPE hello<world<nested>");
        let result = doctype_declaration().easy_parse(PositionStream::new(data.as_str()));
        check_parse_error!(
            result.unwrap_err();
            (0, data.len()),
            Error::Unexpected(Info::Static("end of input")),
            Error::Expected(Info::Range(">")),
            Error::Message(Info::Static("In DOCTYPE declaration"))
        );
    }

    #[test]
    fn test_comment_simple() {
        let data = Buffer::from_str("<!-- some-comment-->");
        let result = comment().easy_parse(PositionStream::new(data.as_str()));
        check_parsed!(result.unwrap(); data; Event::comment(" some-comment"), ParsedHint::None);
    }

    #[test]
    fn test_comment_double_dash_not_allowed() {
        let data = Buffer::from_str("<!-- some--comment-->");
        let result = comment().easy_parse(PositionStream::new(data.as_str()));
        check_parse_error!(
            result.unwrap_err();
            (0, 9),
            Error::Unexpected(Info::Token('-')),
            Error::Expected(Info::Range("-->")),
            Error::Message(Info::Static("In comment"))
        );
    }
}
