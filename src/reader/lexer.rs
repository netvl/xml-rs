//! Contains simple lexer for XML documents.
//!
//! This module is for internal use. Use `xml::pull` module to do parsing.


use crate::reader::ErrorKind;
use crate::reader::error::SyntaxError;
use std::collections::VecDeque;
use std::fmt;
use std::io::Read;
use std::result;
use crate::common::{is_name_char, is_whitespace_char, Position, TextPosition, is_xml10_char, is_xml11_char};
use crate::reader::Error;
use crate::util::{CharReader, Encoding};

use super::ParserConfig2;

/// `Token` represents a single lexeme of an XML document. These lexemes
/// are used to perform actual parsing.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum Token {
    /// `<?`
    ProcessingInstructionStart,
    /// `?>`
    ProcessingInstructionEnd,
    /// `<!DOCTYPE
    DoctypeStart,
    /// `<`
    OpeningTagStart,
    /// `</`
    ClosingTagStart,
    /// `>`
    TagEnd,
    /// `/>`
    EmptyTagEnd,
    /// `<!--`
    CommentStart,
    /// `-->`
    CommentEnd,
    /// Any non-special character except whitespace.
    Character(char),
    /// `=`
    EqualsSign,
    /// `'`
    SingleQuote,
    /// `"`
    DoubleQuote,
    /// `<![CDATA[`
    CDataStart,
    /// `]]>`
    CDataEnd,
    /// `&`
    ReferenceStart,
    /// `;`
    ReferenceEnd,
    /// `<!` of `ENTITY`
    MarkupDeclarationStart,
}

impl fmt::Display for Token {
    #[cold]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Token::Character(c) => c.fmt(f),
            other => match other {
                Token::OpeningTagStart            => "<",
                Token::ProcessingInstructionStart => "<?",
                Token::DoctypeStart               => "<!DOCTYPE",
                Token::ClosingTagStart            => "</",
                Token::CommentStart               => "<!--",
                Token::CDataStart                 => "<![CDATA[",
                Token::TagEnd                     => ">",
                Token::EmptyTagEnd                => "/>",
                Token::ProcessingInstructionEnd   => "?>",
                Token::CommentEnd                 => "-->",
                Token::CDataEnd                   => "]]>",
                Token::ReferenceStart             => "&",
                Token::ReferenceEnd               => ";",
                Token::EqualsSign                 => "=",
                Token::SingleQuote                => "'",
                Token::DoubleQuote                => "\"",
                Token::MarkupDeclarationStart     => "<!",
                _                          => unreachable!()
            }.fmt(f),
        }
    }
}

impl Token {
    pub fn as_static_str(&self) -> Option<&'static str> {
        match *self {
            Token::OpeningTagStart            => Some("<"),
            Token::ProcessingInstructionStart => Some("<?"),
            Token::DoctypeStart               => Some("<!DOCTYPE"),
            Token::ClosingTagStart            => Some("</"),
            Token::CommentStart               => Some("<!--"),
            Token::CDataStart                 => Some("<![CDATA["),
            Token::TagEnd                     => Some(">"),
            Token::EmptyTagEnd                => Some("/>"),
            Token::ProcessingInstructionEnd   => Some("?>"),
            Token::CommentEnd                 => Some("-->"),
            Token::CDataEnd                   => Some("]]>"),
            Token::ReferenceStart             => Some("&"),
            Token::ReferenceEnd               => Some(";"),
            Token::EqualsSign                 => Some("="),
            Token::SingleQuote                => Some("'"),
            Token::DoubleQuote                => Some("\""),
            _                                 => None
        }
    }

    // using String.push_str(token.to_string()) is simply way too slow
    pub fn push_to_string(&self, target: &mut String) {
        match *self {
            Token::Character(c) => {
                debug_assert!(is_xml10_char(c) || is_xml11_char(c));
                target.push(c)
            },
            _ => if let Some(s) = self.as_static_str() {
                target.push_str(s);
            }
        }
    }
}

#[derive(Copy, Clone)]
enum State {
    /// Default state
    Normal,
    /// Triggered on '<'
    TagStarted,
    /// Triggered on '<!'
    CommentOrCDataOrDoctypeStarted,
    /// Triggered on '<!-'
    CommentStarted,
    /// Triggered on '<!D' up to '<!DOCTYPE'
    DoctypeStarted(DoctypeStartedSubstate),
    /// Other items like `<!ELEMENT` in DTD
    InsideMarkupDeclaration,
    /// Triggered after DoctypeStarted to handle sub elements
    InsideDoctype,
    /// Triggered on '<![' up to '<![CDATA'
    CDataStarted(CDataStartedSubstate),
    /// Triggered on '?'
    ProcessingInstructionClosing,
    /// Triggered on '/'
    EmptyTagClosing,
    /// Triggered on '-' up to '--'
    CommentClosing(ClosingSubstate),
    /// Triggered on ']' up to ']]' inside CDATA
    CDataClosing(ClosingSubstate),
    /// Triggered on ']' up to ']]' outside CDATA
    InvalidCDataClosing(ClosingSubstate),
    /// After `<!--`
    InsideComment,
    /// After `<[[`
    InsideCdata,
    /// After `<?`
    InsideProcessingInstruction,
    /// `<!ENTITY "here">`
    InsideMarkupDeclarationQuotedString(QuoteStyle),
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum QuoteStyle {
    Single, Double
}

#[derive(Copy, Clone)]
enum ClosingSubstate {
    First, Second
}

#[derive(Copy, Clone)]
enum DoctypeStartedSubstate {
    D, DO, DOC, DOCT, DOCTY, DOCTYP
}

#[derive(Copy, Clone)]
enum CDataStartedSubstate {
    E, C, CD, CDA, CDAT, CDATA
}

/// `Result` represents lexing result. It is either a token or an error message.
pub(crate) type Result<T = Option<Token>, E = Error> = result::Result<T, E>;

/// Helps to set up a dispatch table for lexing large unambigous tokens like
/// `<![CDATA[` or `<!DOCTYPE `.
macro_rules! dispatch_on_enum_state(
    ($_self:ident, $s:expr, $c:expr, $is:expr,
     $($st:ident; $stc:expr ; $next_st:ident ; $chunk:expr),+;
     $end_st:ident ; $end_c:expr ; $end_chunk:expr ; $e:expr) => (
        match $s {
            $(
            $st => match $c {
                $stc => $_self.move_to($is($next_st)),
                _  => $_self.handle_error($chunk, $c)
            },
            )+
            $end_st => match $c {
                $end_c => $e,
                _      => $_self.handle_error($end_chunk, $c)
            }
        }
    )
);

/// `Lexer` is a lexer for XML documents, which implements pull API.
///
/// Main method is `next_token` which accepts an `std::io::Read` instance and
/// tries to read the next lexeme from it.
///
/// When `skip_errors` flag is set, invalid lexemes will be returned as `Chunk`s.
/// When it is not set, errors will be reported as `Err` objects with a string message.
/// By default this flag is not set. Use `enable_errors` and `disable_errors` methods
/// to toggle the behavior.
pub(crate) struct Lexer {
    st: State,
    reader: CharReader,
    pos: TextPosition,
    head_pos: TextPosition,
    char_queue: VecDeque<char>,
    /// Default state to go back to after a tag end (may be `InsideDoctype`)
    normal_state: State,
    inside_token: bool,
    eof_handled: bool,
    reparse_depth: u8,
    #[cfg(test)]
    skip_errors: bool,

    max_entity_expansion_depth: u8,
    max_entity_expansion_length: usize,
}

impl Position for Lexer {
    #[inline]
    /// Returns the position of the last token produced by the lexer
    fn position(&self) -> TextPosition { self.pos }
}

impl Lexer {
    /// Returns a new lexer with default state.
    pub(crate) fn new(config: &ParserConfig2) -> Lexer {
        Lexer {
            reader: CharReader::new(),
            pos: TextPosition::new(),
            head_pos: TextPosition::new(),
            char_queue: VecDeque::with_capacity(4),  // TODO: check size
            st: State::Normal,
            normal_state: State::Normal,
            inside_token: false,
            eof_handled: false,
            reparse_depth: 0,
            #[cfg(test)]
            skip_errors: false,

            max_entity_expansion_depth: config.max_entity_expansion_depth,
            max_entity_expansion_length: config.max_entity_expansion_length,
        }
    }

    pub(crate) fn encoding(&mut self) -> Encoding {
        self.reader.encoding
    }

    pub(crate) fn set_encoding(&mut self, encoding: Encoding) {
        self.reader.encoding = encoding;
    }

    /// Disables error handling so `next_token` will return `Some(Chunk(..))`
    /// upon invalid lexeme with this lexeme content.
    #[cfg(test)] fn disable_errors(&mut self) { self.skip_errors = true; }

    /// Reset the eof handled flag of the lexer.
    #[inline]
    pub fn reset_eof_handled(&mut self) { self.eof_handled = false; }

    /// Tries to read the next token from the buffer.
    ///
    /// It is possible to pass different instaces of `BufReader` each time
    /// this method is called, but the resulting behavior is undefined in this case.
    ///
    /// Return value:
    /// * `Err(reason) where reason: reader::Error` - when an error occurs;
    /// * `Ok(None)` - upon end of stream is reached;
    /// * `Ok(Some(token)) where token: Token` - in case a complete-token has been read from the stream.
    pub fn next_token<B: Read>(&mut self, b: &mut B) -> Result {
        // Already reached end of buffer
        if self.eof_handled {
            return Ok(None);
        }

        if !self.inside_token {
            self.pos = self.head_pos;
            self.inside_token = true;
        }

        // Check if we have saved a char or two for ourselves
        while let Some(c) = self.char_queue.pop_front() {
            match self.dispatch_char(c)? {
                Some(t) => {
                    self.inside_token = false;
                    return Ok(Some(t));
                }
                None => {} // continue
            }
        }
        // if char_queue is empty, all circular reparsing is done
        self.reparse_depth = 0;
        loop {
            let c = match self.reader.next_char_from(b)? {
                Some(c) => c,  // got next char
                None => break, // nothing to read left
            };

            if c == '\n' {
                self.head_pos.new_line();
            } else {
                self.head_pos.advance(1);
            }

            match self.dispatch_char(c)? {
                Some(t) => {
                    self.inside_token = false;
                    return Ok(Some(t));
                }
                None => {
                    // continue
                }
            }
        }

        self.end_of_stream()
    }

    #[inline(never)]
    fn end_of_stream(&mut self) -> Result {
        // Handle end of stream
        self.eof_handled = true;
        self.pos = self.head_pos;
        match self.st {
            State::InsideCdata | State::CDataClosing(_) => Err(self.error(SyntaxError::UnclosedCdata)),
            State::TagStarted | State::CommentOrCDataOrDoctypeStarted |
            State::CommentStarted | State::CDataStarted(_)| State::DoctypeStarted(_) |
            State::CommentClosing(ClosingSubstate::Second) |
            State::InsideComment | State::InsideMarkupDeclaration |
            State::InsideProcessingInstruction | State::ProcessingInstructionClosing |
            State::InsideDoctype | State::InsideMarkupDeclarationQuotedString(_) =>
                Err(self.error(SyntaxError::UnexpectedEof)),
            State::EmptyTagClosing =>
                Ok(Some(Token::Character('/'))),
            State::CommentClosing(ClosingSubstate::First) =>
                Ok(Some(Token::Character('-'))),
            State::InvalidCDataClosing(ClosingSubstate::First) =>
                Ok(Some(Token::Character(']'))),
            State::InvalidCDataClosing(ClosingSubstate::Second) => {
                self.eof_handled = false;
                self.move_to_with_unread(State::Normal, &[']'], Token::Character(']'))
            },
            State::Normal =>
                Ok(None),
        }
    }

    #[cold]
    fn error(&self, e: SyntaxError) -> Error {
        Error {
            pos: self.position(),
            kind: ErrorKind::Syntax(e.to_cow()),
        }
    }


    #[inline(never)]
    fn dispatch_char(&mut self, c: char) -> Result {
        match self.st {
            State::Normal                         => self.normal(c),
            State::TagStarted                     => self.tag_opened(c),
            State::EmptyTagClosing                => self.empty_element_closing(c),
            State::CommentOrCDataOrDoctypeStarted => self.comment_or_cdata_or_doctype_started(c),
            State::InsideCdata                    => self.inside_cdata(c),
            State::CDataStarted(s)                => self.cdata_started(c, s),
            State::InsideComment                  => self.inside_comment_state(c),
            State::CommentStarted                 => self.comment_started(c),
            State::InsideProcessingInstruction    => self.inside_processing_instruction(c),
            State::ProcessingInstructionClosing   => self.processing_instruction_closing(c),
            State::CommentClosing(s)              => self.comment_closing(c, s),
            State::CDataClosing(s)                => self.cdata_closing(c, s),
            State::InsideDoctype                  => self.inside_doctype(c),
            State::DoctypeStarted(s)              => self.doctype_started(c, s),
            State::InvalidCDataClosing(s)         => self.invalid_cdata_closing(c, s),
            State::InsideMarkupDeclaration        => self.markup_declaration(c),
            State::InsideMarkupDeclarationQuotedString(q) => self.markup_declaration_string(c, q),
        }
    }

    #[inline]
    fn move_to(&mut self, st: State) -> Result {
        self.st = st;
        Ok(None)
    }

    #[inline]
    fn move_to_with(&mut self, st: State, token: Token) -> Result {
        self.st = st;
        Ok(Some(token))
    }

    #[inline]
    fn move_to_and_reset_normal(&mut self, st: State, token: Token) -> Result {
        self.normal_state = st;
        self.st = st;
        Ok(Some(token))
    }

    fn move_to_with_unread(&mut self, st: State, cs: &[char], token: Token) -> Result {
        for c in cs.iter().rev().copied() {
            self.char_queue.push_front(c);
        }
        self.move_to_with(st, token)
    }

    pub(crate) fn reparse(&mut self, markup: &str) -> Result<()> {
        if markup.is_empty() {
            return Ok(());
        }

        self.reparse_depth += 1;
        if self.reparse_depth > self.max_entity_expansion_depth || self.char_queue.len() > self.max_entity_expansion_length {
            return Err(self.error(SyntaxError::EntityTooBig))
        }

        self.eof_handled = false;
        self.char_queue.reserve(markup.len());
        for c in markup.chars().rev() {
            self.char_queue.push_front(c);
        }

        Ok(())
    }

    fn handle_error(&mut self, chunk: &'static str, c: char) -> Result {
        debug_assert!(!chunk.is_empty());

        #[cfg(test)]
        if self.skip_errors {
            let mut chars = chunk.chars();
            let first = chars.next().unwrap_or('\0');
            self.char_queue.extend(chars);
            self.char_queue.push_back(c);
            return self.move_to_with(State::Normal, Token::Character(first));
        }
        Err(self.error(SyntaxError::UnexpectedTokenBefore(chunk, c)))
    }

    /// Encountered a char
    fn normal(&mut self, c: char) -> Result {
        match c {
            '<'                        => self.move_to(State::TagStarted),
            '>'                        => Ok(Some(Token::TagEnd)),
            '/'                        => self.move_to(State::EmptyTagClosing),
            '='                        => Ok(Some(Token::EqualsSign)),
            '"'                        => Ok(Some(Token::DoubleQuote)),
            '\''                       => Ok(Some(Token::SingleQuote)),
            ']'                        => self.move_to(State::InvalidCDataClosing(ClosingSubstate::First)),
            '&'                        => Ok(Some(Token::ReferenceStart)),
            ';'                        => Ok(Some(Token::ReferenceEnd)),
            _                          => Ok(Some(Token::Character(c)))
        }
    }

    fn inside_cdata(&mut self, c: char) -> Result {
        match c {
            ']'                        => self.move_to(State::CDataClosing(ClosingSubstate::First)),
            _                          => Ok(Some(Token::Character(c)))
        }
    }

    fn inside_processing_instruction(&mut self, c: char) -> Result {
        // These tokens are used by `<?xml?>` parser
        match c {
            '?'                        => self.move_to(State::ProcessingInstructionClosing),
            '<'                        => Ok(Some(Token::OpeningTagStart)),
            '>'                        => Ok(Some(Token::TagEnd)),
            '/'                        => Ok(Some(Token::ClosingTagStart)),
            '='                        => Ok(Some(Token::EqualsSign)),
            '"'                        => Ok(Some(Token::DoubleQuote)),
            '\''                       => Ok(Some(Token::SingleQuote)),
            '&'                        => Ok(Some(Token::ReferenceStart)),
            ';'                        => Ok(Some(Token::ReferenceEnd)),
            _                          => Ok(Some(Token::Character(c)))
        }
    }

    fn inside_comment_state(&mut self, c: char) -> Result {
        match c {
            '-'                        => self.move_to(State::CommentClosing(ClosingSubstate::First)),
            _                          => Ok(Some(Token::Character(c)))
        }
    }

    /// Encountered '<'
    fn tag_opened(&mut self, c: char) -> Result {
        match c {
            '?'                        => self.move_to_with(State::InsideProcessingInstruction, Token::ProcessingInstructionStart),
            '/'                        => self.move_to_with(self.normal_state, Token::ClosingTagStart),
            '!'                        => self.move_to(State::CommentOrCDataOrDoctypeStarted),
            _ if is_whitespace_char(c) => self.move_to_with_unread(self.normal_state, &[c], Token::OpeningTagStart),
            _ if is_name_char(c)       => self.move_to_with_unread(self.normal_state, &[c], Token::OpeningTagStart),
            _                          => self.handle_error("<", c)
        }
    }

    /// Encountered '<!'
    fn comment_or_cdata_or_doctype_started(&mut self, c: char) -> Result {
        match c {
            '-' => self.move_to(State::CommentStarted),
            '[' => self.move_to(State::CDataStarted(CDataStartedSubstate::E)),
            'D' => self.move_to(State::DoctypeStarted(DoctypeStartedSubstate::D)),
            'E' | 'A' | 'N' if matches!(self.normal_state, State::InsideDoctype) => {
                self.move_to_with_unread(State::InsideMarkupDeclaration, &[c], Token::MarkupDeclarationStart)
            },
            _ => self.handle_error("<!", c),
        }
    }

    /// Encountered '<!-'
    fn comment_started(&mut self, c: char) -> Result {
        match c {
            '-' => self.move_to_with(State::InsideComment, Token::CommentStart),
            _ => self.handle_error("<!-", c),
        }
    }

    /// Encountered '<!['
    fn cdata_started(&mut self, c: char, s: CDataStartedSubstate) -> Result {
        use self::CDataStartedSubstate::{C, CD, CDA, CDAT, CDATA, E};
        dispatch_on_enum_state!(self, s, c, State::CDataStarted,
            E     ; 'C' ; C     ; "<![",
            C     ; 'D' ; CD    ; "<![C",
            CD    ; 'A' ; CDA   ; "<![CD",
            CDA   ; 'T' ; CDAT  ; "<![CDA",
            CDAT  ; 'A' ; CDATA ; "<![CDAT";
            CDATA ; '[' ; "<![CDATA" ; self.move_to_with(State::InsideCdata, Token::CDataStart)
        )
    }

    /// Encountered '<!…' that isn't DOCTYPE or CDATA
    fn markup_declaration(&mut self, c: char) -> Result {
        match c {
            '<'                        => self.handle_error("<!", c),
            '>'                        => self.move_to_with(self.normal_state, Token::TagEnd),
            '&'                        => Ok(Some(Token::ReferenceStart)),
            ';'                        => Ok(Some(Token::ReferenceEnd)),
            '"'                        => self.move_to_with(State::InsideMarkupDeclarationQuotedString(QuoteStyle::Double), Token::DoubleQuote),
            '\''                       => self.move_to_with(State::InsideMarkupDeclarationQuotedString(QuoteStyle::Single), Token::SingleQuote),
            _                          => Ok(Some(Token::Character(c))),
        }
    }

    fn markup_declaration_string(&mut self, c: char, q: QuoteStyle) -> Result {
        match c {
            '"' if q == QuoteStyle::Double  => self.move_to_with(State::InsideMarkupDeclaration, Token::DoubleQuote),
            '\'' if q == QuoteStyle::Single => self.move_to_with(State::InsideMarkupDeclaration, Token::SingleQuote),
            _                               => Ok(Some(Token::Character(c))),
        }
    }

    /// Encountered '<!D'
    fn doctype_started(&mut self, c: char, s: DoctypeStartedSubstate) -> Result {
        use self::DoctypeStartedSubstate::{D, DO, DOC, DOCT, DOCTY, DOCTYP};
        dispatch_on_enum_state!(self, s, c, State::DoctypeStarted,
            D      ; 'O' ; DO     ; "<!D",
            DO     ; 'C' ; DOC    ; "<!DO",
            DOC    ; 'T' ; DOCT   ; "<!DOC",
            DOCT   ; 'Y' ; DOCTY  ; "<!DOCT",
            DOCTY  ; 'P' ; DOCTYP ; "<!DOCTY";
            DOCTYP ; 'E' ; "<!DOCTYP" ; self.move_to_and_reset_normal(State::InsideDoctype, Token::DoctypeStart)
        )
    }

    /// State used while awaiting the closing bracket for the <!DOCTYPE tag
    fn inside_doctype(&mut self, c: char) -> Result {
        match c {
            '>' => self.move_to_and_reset_normal(State::Normal, Token::TagEnd),
            '<'                        => self.move_to(State::TagStarted),
            '&'                        => Ok(Some(Token::ReferenceStart)),
            ';'                        => Ok(Some(Token::ReferenceEnd)),
            '"'                        => Ok(Some(Token::DoubleQuote)),
            '\''                       => Ok(Some(Token::SingleQuote)),
            _                          => Ok(Some(Token::Character(c))),
        }
    }

    /// Encountered '?'
    fn processing_instruction_closing(&mut self, c: char) -> Result {
        match c {
            '>' => self.move_to_with(self.normal_state, Token::ProcessingInstructionEnd),
            _ => self.move_to_with_unread(State::InsideProcessingInstruction, &[c], Token::Character('?')),
        }
    }

    /// Encountered '/'
    fn empty_element_closing(&mut self, c: char) -> Result {
        match c {
            '>' => self.move_to_with(self.normal_state, Token::EmptyTagEnd),
            _ => self.move_to_with_unread(self.normal_state, &[c], Token::Character('/')),
        }
    }

    /// Encountered '-'
    fn comment_closing(&mut self, c: char, s: ClosingSubstate) -> Result {
        match s {
            ClosingSubstate::First => match c {
                '-' => self.move_to(State::CommentClosing(ClosingSubstate::Second)),
                _ => self.move_to_with_unread(State::InsideComment, &[c], Token::Character('-')),
            },
            ClosingSubstate::Second => match c {
                '>' => self.move_to_with(self.normal_state, Token::CommentEnd),
                // double dash not followed by a greater-than is a hard error inside comment
                _ => self.handle_error("--", c),
            },
        }
    }

    /// Encountered ']'
    fn cdata_closing(&mut self, c: char, s: ClosingSubstate) -> Result {
        match s {
            ClosingSubstate::First => match c {
                ']' => self.move_to(State::CDataClosing(ClosingSubstate::Second)),
                _ => self.move_to_with_unread(State::InsideCdata, &[c], Token::Character(']')),
            },
            ClosingSubstate::Second => match c {
                '>' => self.move_to_with(State::Normal, Token::CDataEnd),
                _ => self.move_to_with_unread(State::InsideCdata, &[']', c], Token::Character(']')),
            },
        }
    }

    /// Encountered ']'
    fn invalid_cdata_closing(&mut self, c: char, s: ClosingSubstate) -> Result {
        match s {
            ClosingSubstate::First => match c {
                ']' => self.move_to(State::InvalidCDataClosing(ClosingSubstate::Second)),
                _ => self.move_to_with_unread(State::Normal, &[c], Token::Character(']')),
            },
            ClosingSubstate::Second => match c {
                '>' => self.move_to_with(self.normal_state, Token::CDataEnd),
                _ => self.move_to_with_unread(State::Normal, &[']', c], Token::Character(']')),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{common::Position, reader::ParserConfig2};
    use std::io::{BufReader, Cursor};

    use super::{Lexer, Token};

    macro_rules! assert_oks(
        (for $lex:ident and $buf:ident ; $($e:expr)+) => ({
            $(
                assert_eq!(Ok(Some($e)), $lex.next_token(&mut $buf));
             )+
        })
    );

    macro_rules! assert_err(
        (for $lex:ident and $buf:ident expect row $r:expr ; $c:expr, $s:expr) => ({
            let err = $lex.next_token(&mut $buf);
            assert!(err.is_err());
            let err = err.unwrap_err();
            assert_eq!($r as u64, err.position().row);
            assert_eq!($c as u64, err.position().column);
        })
    );

    macro_rules! assert_none(
        (for $lex:ident and $buf:ident) => (
            assert_eq!(Ok(None), $lex.next_token(&mut $buf))
        )
    );

    fn make_lex_and_buf(s: &str) -> (Lexer, BufReader<Cursor<Vec<u8>>>) {
        (Lexer::new(&ParserConfig2::default()), BufReader::new(Cursor::new(s.to_owned().into_bytes())))
    }

    #[test]
    fn tricky_pi() {
        let (mut lex, mut buf) = make_lex_and_buf(r#"<?x<!-- &??><x>"#);

        assert_oks!(for lex and buf ;
            Token::ProcessingInstructionStart
            Token::Character('x')
            Token::OpeningTagStart // processing of <?xml?> relies on the extra tokens
            Token::Character('!')
            Token::Character('-')
            Token::Character('-')
            Token::Character(' ')
            Token::ReferenceStart
            Token::Character('?')
            Token::ProcessingInstructionEnd
            Token::OpeningTagStart
            Token::Character('x')
            Token::TagEnd
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn reparser() {
        let (mut lex, mut buf) = make_lex_and_buf(r#"&a;"#);

        assert_oks!(for lex and buf ;
            Token::ReferenceStart
            Token::Character('a')
            Token::ReferenceEnd
        );
        lex.reparse("<hi/>").unwrap();
        assert_oks!(for lex and buf ;
            Token::OpeningTagStart
            Token::Character('h')
            Token::Character('i')
            Token::EmptyTagEnd
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn simple_lexer_test() {
        let (mut lex, mut buf) = make_lex_and_buf(
            r#"<a p='q'> x<b z="y">d	</b></a><p/> <?nm ?> <!-- a c --> &nbsp;"#
        );

        assert_oks!(for lex and buf ;
            Token::OpeningTagStart
            Token::Character('a')
            Token::Character(' ')
            Token::Character('p')
            Token::EqualsSign
            Token::SingleQuote
            Token::Character('q')
            Token::SingleQuote
            Token::TagEnd
            Token::Character(' ')
            Token::Character('x')
            Token::OpeningTagStart
            Token::Character('b')
            Token::Character(' ')
            Token::Character('z')
            Token::EqualsSign
            Token::DoubleQuote
            Token::Character('y')
            Token::DoubleQuote
            Token::TagEnd
            Token::Character('d')
            Token::Character('\t')
            Token::ClosingTagStart
            Token::Character('b')
            Token::TagEnd
            Token::ClosingTagStart
            Token::Character('a')
            Token::TagEnd
            Token::OpeningTagStart
            Token::Character('p')
            Token::EmptyTagEnd
            Token::Character(' ')
            Token::ProcessingInstructionStart
            Token::Character('n')
            Token::Character('m')
            Token::Character(' ')
            Token::ProcessingInstructionEnd
            Token::Character(' ')
            Token::CommentStart
            Token::Character(' ')
            Token::Character('a')
            Token::Character(' ')
            Token::Character('c')
            Token::Character(' ')
            Token::CommentEnd
            Token::Character(' ')
            Token::ReferenceStart
            Token::Character('n')
            Token::Character('b')
            Token::Character('s')
            Token::Character('p')
            Token::ReferenceEnd
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn special_chars_test() {
        let (mut lex, mut buf) = make_lex_and_buf(
            r#"?x!+ // -| ]z]]"#
        );

        assert_oks!(for lex and buf ;
            Token::Character('?')
            Token::Character('x')
            Token::Character('!')
            Token::Character('+')
            Token::Character(' ')
            Token::Character('/')
            Token::Character('/')
            Token::Character(' ')
            Token::Character('-')
            Token::Character('|')
            Token::Character(' ')
            Token::Character(']')
            Token::Character('z')
            Token::Character(']')
            Token::Character(']')
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn cdata_test() {
        let (mut lex, mut buf) = make_lex_and_buf(
            r#"<a><![CDATA[x y ?]]> </a>"#
        );

        assert_oks!(for lex and buf ;
            Token::OpeningTagStart
            Token::Character('a')
            Token::TagEnd
            Token::CDataStart
            Token::Character('x')
            Token::Character(' ')
            Token::Character('y')
            Token::Character(' ')
            Token::Character('?')
            Token::CDataEnd
            Token::Character(' ')
            Token::ClosingTagStart
            Token::Character('a')
            Token::TagEnd
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn cdata_closers_test() {
        let (mut lex, mut buf) = make_lex_and_buf(
            r#"<![CDATA[] > ]> ]]><!---->]]<a>"#
        );

        assert_oks!(for lex and buf ;
            Token::CDataStart
            Token::Character(']')
            Token::Character(' ')
            Token::Character('>')
            Token::Character(' ')
            Token::Character(']')
            Token::Character('>')
            Token::Character(' ')
            Token::CDataEnd
            Token::CommentStart
            Token::CommentEnd
            Token::Character(']')
            Token::Character(']')
            Token::OpeningTagStart
            Token::Character('a')
            Token::TagEnd
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn doctype_test() {
        let (mut lex, mut buf) = make_lex_and_buf(
            r#"<a><!DOCTYPE ab xx z> "#
        );
        assert_oks!(for lex and buf ;
            Token::OpeningTagStart
            Token::Character('a')
            Token::TagEnd
            Token::DoctypeStart
            Token::Character(' ')
            Token::Character('a')
            Token::Character('b')
            Token::Character(' ')
            Token::Character('x')
            Token::Character('x')
            Token::Character(' ')
            Token::Character('z')
            Token::TagEnd
            Token::Character(' ')
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn tricky_comments() {
        let (mut lex, mut buf) = make_lex_and_buf(
            r#"<a><!-- C ->--></a>"#
        );
        assert_oks!(for lex and buf ;
            Token::OpeningTagStart
            Token::Character('a')
            Token::TagEnd
            Token::CommentStart
            Token::Character(' ')
            Token::Character('C')
            Token::Character(' ')
            Token::Character('-')
            Token::Character('>')
            Token::CommentEnd
            Token::ClosingTagStart
            Token::Character('a')
            Token::TagEnd
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn doctype_with_internal_subset_test() {
        let (mut lex, mut buf) = make_lex_and_buf(
            r#"<a><!DOCTYPE ab[<!ELEMENT ba ">>>"> ]> "#
        );
        assert_oks!(for lex and buf ;
            Token::OpeningTagStart
            Token::Character('a')
            Token::TagEnd
            Token::DoctypeStart
            Token::Character(' ')
            Token::Character('a')
            Token::Character('b')
            Token::Character('[')
            Token::MarkupDeclarationStart
            Token::Character('E')
            Token::Character('L')
            Token::Character('E')
            Token::Character('M')
            Token::Character('E')
            Token::Character('N')
            Token::Character('T')
            Token::Character(' ')
            Token::Character('b')
            Token::Character('a')
            Token::Character(' ')
            Token::DoubleQuote
            Token::Character('>')
            Token::Character('>')
            Token::Character('>')
            Token::DoubleQuote
            Token::TagEnd
            Token::Character(' ')
            Token::Character(']')
            Token::TagEnd
            Token::Character(' ')
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn doctype_internal_pi_comment() {
        let (mut lex, mut buf) = make_lex_and_buf(
            "<!DOCTYPE a [\n<!ELEMENT l ANY> <!-- <?non?>--> <?pi > ?> \n]>"
        );
        assert_oks!(for lex and buf ;
            Token::DoctypeStart
            Token::Character(' ')
            Token::Character('a')
            Token::Character(' ')
            Token::Character('[')
            Token::Character('\n')
            Token::MarkupDeclarationStart
            Token::Character('E')
            Token::Character('L')
            Token::Character('E')
            Token::Character('M')
            Token::Character('E')
            Token::Character('N')
            Token::Character('T')
            Token::Character(' ')
            Token::Character('l')
            Token::Character(' ')
            Token::Character('A')
            Token::Character('N')
            Token::Character('Y')
            Token::TagEnd
            Token::Character(' ')
            Token::CommentStart
            Token::Character(' ')
            Token::Character('<')
            Token::Character('?')
            Token::Character('n')
            Token::Character('o')
            Token::Character('n')
            Token::Character('?')
            Token::Character('>')
            Token::CommentEnd
            Token::Character(' ')
            Token::ProcessingInstructionStart
            Token::Character('p')
            Token::Character('i')
            Token::Character(' ')
            Token::TagEnd // not really
            Token::Character(' ')
            Token::ProcessingInstructionEnd
            Token::Character(' ')
            Token::Character('\n')
            Token::Character(']')
            Token::TagEnd // DTD
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn end_of_stream_handling_ok() {
        macro_rules! eof_check(
            ($data:expr ; $token:expr) => ({
                let (mut lex, mut buf) = make_lex_and_buf($data);
                assert_oks!(for lex and buf ; $token);
                assert_none!(for lex and buf);
            })
        );
        eof_check!("?"  ; Token::Character('?'));
        eof_check!("/"  ; Token::Character('/'));
        eof_check!("-"  ; Token::Character('-'));
        eof_check!("]"  ; Token::Character(']'));
        eof_check!("]"  ; Token::Character(']'));
        eof_check!("]"  ; Token::Character(']'));
    }

    #[test]
    fn end_of_stream_handling_error() {
        macro_rules! eof_check(
            ($data:expr; $r:expr, $c:expr) => ({
                let (mut lex, mut buf) = make_lex_and_buf($data);
                assert_err!(for lex and buf expect row $r ; $c, "Unexpected end of stream");
                assert_none!(for lex and buf);
            })
        );
        eof_check!("<"        ; 0, 1);
        eof_check!("<!"       ; 0, 2);
        eof_check!("<!-"      ; 0, 3);
        eof_check!("<!["      ; 0, 3);
        eof_check!("<![C"     ; 0, 4);
        eof_check!("<![CD"    ; 0, 5);
        eof_check!("<![CDA"   ; 0, 6);
        eof_check!("<![CDAT"  ; 0, 7);
        eof_check!("<![CDATA" ; 0, 8);
    }

    #[test]
    fn error_in_comment_or_cdata_prefix() {
        let (mut lex, mut buf) = make_lex_and_buf("<!x");
        assert_err!(for lex and buf expect row 0 ; 0,
            "Unexpected token '<!' before 'x'"
        );

        let (mut lex, mut buf) = make_lex_and_buf("<!x");
        lex.disable_errors();
        assert_oks!(for lex and buf ;
            Token::Character('<')
            Token::Character('!')
            Token::Character('x')
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn error_in_comment_started() {
        let (mut lex, mut buf) = make_lex_and_buf("<!-\t");
        assert_err!(for lex and buf expect row 0 ; 0,
            "Unexpected token '<!-' before '\t'"
        );

        let (mut lex, mut buf) = make_lex_and_buf("<!-\t");
        lex.disable_errors();
        assert_oks!(for lex and buf ;
            Token::Character('<')
            Token::Character('!')
            Token::Character('-')
            Token::Character('\t')
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn error_in_comment_two_dashes_not_at_end() {
        let (mut lex, mut buf) = make_lex_and_buf("--x");
        lex.st = super::State::InsideComment;
        assert_err!(for lex and buf expect row 0; 0,
            "Unexpected token '--' before 'x'"
        );

        let (mut lex, mut buf) = make_lex_and_buf("--x");
        assert_oks!(for lex and buf ;
            Token::Character('-')
            Token::Character('-')
            Token::Character('x')
        );
    }

    macro_rules! check_case(
        ($chunk:expr, $app:expr; $data:expr; $r:expr, $c:expr, $s:expr) => ({
            let (mut lex, mut buf) = make_lex_and_buf($data);
            assert_err!(for lex and buf expect row $r ; $c, $s);

            let (mut lex, mut buf) = make_lex_and_buf($data);
            lex.disable_errors();
            for c in $chunk.chars() {
                assert_eq!(Ok(Some(Token::Character(c))), lex.next_token(&mut buf));
            }
            assert_oks!(for lex and buf ;
                Token::Character($app)
            );
            assert_none!(for lex and buf);
        })
    );

    #[test]
    fn token_size() {
        assert_eq!(4, std::mem::size_of::<Token>());
        assert_eq!(2, std::mem::size_of::<super::State>());
    }

    #[test]
    fn error_in_cdata_started() {
        check_case!("<![",      '['; "<![["      ; 0, 0, "Unexpected token '<![' before '['");
        check_case!("<![C",     '['; "<![C["     ; 0, 0, "Unexpected token '<![C' before '['");
        check_case!("<![CD",    '['; "<![CD["    ; 0, 0, "Unexpected token '<![CD' before '['");
        check_case!("<![CDA",   '['; "<![CDA["   ; 0, 0, "Unexpected token '<![CDA' before '['");
        check_case!("<![CDAT",  '['; "<![CDAT["  ; 0, 0, "Unexpected token '<![CDAT' before '['");
        check_case!("<![CDATA", '|'; "<![CDATA|" ; 0, 0, "Unexpected token '<![CDATA' before '|'");
    }

    #[test]
    fn error_in_doctype_started() {
        check_case!("<!D",      'a'; "<!Da"      ; 0, 0, "Unexpected token '<!D' before 'a'");
        check_case!("<!DO",     'b'; "<!DOb"     ; 0, 0, "Unexpected token '<!DO' before 'b'");
        check_case!("<!DOC",    'c'; "<!DOCc"    ; 0, 0, "Unexpected token '<!DOC' before 'c'");
        check_case!("<!DOCT",   'd'; "<!DOCTd"   ; 0, 0, "Unexpected token '<!DOCT' before 'd'");
        check_case!("<!DOCTY",  'e'; "<!DOCTYe"  ; 0, 0, "Unexpected token '<!DOCTY' before 'e'");
        check_case!("<!DOCTYP", 'f'; "<!DOCTYPf" ; 0, 0, "Unexpected token '<!DOCTYP' before 'f'");
    }



    #[test]
    fn issue_98_cdata_ending_with_right_bracket() {
        let (mut lex, mut buf) = make_lex_and_buf(
            r#"<![CDATA[Foo [Bar]]]>"#
        );

        assert_oks!(for lex and buf ;
            Token::CDataStart
            Token::Character('F')
            Token::Character('o')
            Token::Character('o')
            Token::Character(' ')
            Token::Character('[')
            Token::Character('B')
            Token::Character('a')
            Token::Character('r')
            Token::Character(']')
            Token::CDataEnd
        );
        assert_none!(for lex and buf);
    }
}
