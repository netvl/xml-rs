use std::util;

use common::{Error, HasPosition, is_whitespace_char, is_name_char};

/// `Token` represents a single lexeme of an XML document. These lexemes
/// are used to perform actual parsing.
#[deriving(Clone, Eq)]
pub enum Token {
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
    /// A chunk of characters, used for errors recovery.
    Chunk(~str),
    /// Any non-special character except whitespace.
    Character(char),
    /// Whitespace character.
    Whitespace(char),
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
}

impl ToStr for Token {
    fn to_str(&self) -> ~str {
        match *self {
            Chunk(ref s)  => s.clone(),
            Character(c)  => c.to_str(),
            Whitespace(c) => c.to_str(),
            _ => match *self {
                OpeningTagStart            => ~"<",
                ProcessingInstructionStart => ~"<?",
                DoctypeStart               => ~"<!DOCTYPE",
                ClosingTagStart            => ~"</",
                CommentStart               => ~"<!--",
                CDataStart                 => ~"<![CDATA[",
                TagEnd                     => ~">",
                EmptyTagEnd                => ~"/>",
                ProcessingInstructionEnd   => ~"?>",
                CommentEnd                 => ~"-->",
                CDataEnd                   => ~"]]>",
                ReferenceStart             => ~"&",
                ReferenceEnd               => ~";",
                EqualsSign                 => ~"=",
                SingleQuote                => ~"'",
                DoubleQuote                => ~"\"",
                _                          => unreachable!()
            }
        }
    }
}

impl Token {
    /// Returns `true` if this token contains data that can be interpreted
    /// as a part of the text. Surprisingly, this also means '>' and '=' and '"' and "'".
    #[inline]
    pub fn contains_char_data(&self) -> bool {
        match *self {
            Whitespace(_) | Chunk(_) | Character(_) | 
            TagEnd | EqualsSign | DoubleQuote | SingleQuote => true,
            _ => false
        }
    }

    /// Returns `true` if this token corresponds to a white space character.
    #[inline]
    pub fn is_whitespace(&self) -> bool {
        match *self {
            Whitespace(_) => true,
            _ => false
        }
    }
}

enum State {
    /// Triggered on '<'
    TagOpened,
    /// Triggered on '<!'
    CommentOrCDataOrDoctypeStarted,
    /// Triggered on '<!-'
    CommentStarted,
    /// Triggered on '<!D' up to '<!DOCTYPE'
    DoctypeStarted(DoctypeStartedSubstate),
    /// Triggered on '<![' up to '<![CDATA'
    CDataStarted(CDataStartedSubstate),
    /// Triggered on '?'
    ProcessingInstructionClosing,
    /// Triggered on '/'
    EmptyTagClosing,
    /// Triggered on '-' up to '--'
    CommentClosing(ClosingSubstate),
    /// Triggered on ']' up to ']]'
    CDataClosing(ClosingSubstate),
    /// Default state
    Normal
}

enum ClosingSubstate {
    First, Second
}

enum DoctypeStartedSubstate {
    D, DO, DOC, DOCT, DOCTY, DOCTYP, DOCTYPE
}

enum CDataStartedSubstate {
    E, C, CD, CDA, CDAT, CDATA
}

/// `LexResult` represents lexing result. It is either a token or an error message.
pub type LexResult = Result<Token, Error>;

type LexStep = Option<LexResult>;  // TODO: make up with better name

macro_rules! self_error(
    ($msg:expr) => (
        self.error($msg.to_owned())
    );
    ($msg:expr, $($arg:expr),*) => (
        self.error(format!($msg, $($arg),*))
    )
)

/// Helps to set up a dispatch table for lexing large unambigous tokens like
/// `<![CDATA[` or `<!DOCTYPE `.
macro_rules! dispatch_on_enum_state(
    ($s:expr, $c:expr, $is:ident, 
     $($st:ident -> $stc:pat -> $next_st:ident ! $chunk:expr),+; 
     $end_st:ident -> $end_c:pat ! $end_chunk:expr -> $e:expr) => (
        match $s {
            $(
            $st => match $c {
                $stc => self.move_to($is($next_st)),
                _  => self.handle_error(~$chunk, $c)
            },
            )+
            $end_st => match $c {
                $end_c => $e,
                _      => self.handle_error(~$end_chunk, $c)
            }
        }
    )
)

/// `PullLexer` is a lexer for XML documents, which implements pull API.
///
/// Main method is `next_token` which accepts an `std::io::Buffer` and
/// tries to read the next lexeme from it. 
///
/// When `skip_errors` flag is set, invalid lexemes will be returned as `Chunk`s.
/// When it is not set, errors will be reported as `Err` objects with a string message.
/// By default this flag is not set. Use `enable_errors` and `disable_errors` methods
/// to toggle the behavior.
pub struct PullLexer {
    priv row: uint,
    priv col: uint,
    priv temp_char: Option<char>,
    priv st: State,
    priv skip_errors: bool,
    priv eof_handled: bool
}

/// Returns a new lexer with default state.
pub fn new() -> PullLexer {
    PullLexer {
        row: 0,
        col: 0,
        temp_char: None,
        st: Normal,
        skip_errors: false,
        eof_handled: false
    }
}

impl HasPosition for PullLexer {
    /// Returns current row in the input document.
    #[inline]
    fn row(&self) -> uint { self.row }

    /// Returns current column in the document.
    #[inline]
    fn col(&self) -> uint { self.col }
}

impl PullLexer {
    /// Enables error handling so `next_token` will return `Some(Err(..))`
    /// upon invalid lexeme.
    #[inline]
    pub fn enable_errors(&mut self) { self.skip_errors = false; }

    /// Disables error handling so `next_token` will return `Some(Chunk(..))`
    /// upon invalid lexeme with this lexeme content.
    #[inline]
    pub fn disable_errors(&mut self) { self.skip_errors = true; }

    /// Tries to read next token from the buffer.
    ///
    /// It is possible to pass different instaces of `Buffer` each time
    /// this method is called, but the resulting behavior is undefined.
    ///
    /// Returns `None` when logical end of stream is encountered, that is,
    /// after `b.read_char()` returns `None` and the current state is 
    /// is exhausted.
    pub fn next_token<B: Buffer>(&mut self, b: &mut B) -> Option<LexResult> {
        // Already reached end of buffer
        if self.eof_handled {
            return None;
        }

        // Check if we have saved a char for ourselves
        if self.temp_char.is_some() {
            let c = util::replace(&mut self.temp_char, None).unwrap();
            match self.read_next_token(c) {
                Some(t) => return Some(t),
                None => {}  // continue
            }
        }

        // Read more data from the buffer
        for_each!(c in b.read_char() {
            match self.read_next_token(c) {
                Some(t) => return Some(t),
                None    => {}  // continue
            }
        })

        // Handle end of stream
        self.eof_handled = true;
        match self.st {
            TagOpened | CommentOrCDataOrDoctypeStarted | 
            CommentStarted | CDataStarted(_)| DoctypeStarted(_) |
            CommentClosing(Second)  => 
                Some(Err(self_error!("Unexpected end of stream"))),
            ProcessingInstructionClosing =>
                Some(Ok(Character('?'))),
            EmptyTagClosing =>
                Some(Ok(Character('/'))),
            CommentClosing(First) =>
                Some(Ok(Character('-'))),
            CDataClosing(First) =>
                Some(Ok(Character(']'))),
            CDataClosing(Second) =>
                Some(Ok(Chunk(~"]]"))),
            Normal =>
                None
        }
    }

    fn error(&self, msg: ~str) -> Error {
        Error::new(self, msg)
    }

    fn read_next_token(&mut self, c: char) -> LexStep {
        if c == '\n' {
            self.row += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }

        self.dispatch_char(c)
    }


    fn dispatch_char(&mut self, c: char) -> LexStep {
        match self.st {
            Normal                         => self.normal(c),
            TagOpened                      => self.tag_opened(c),
            CommentOrCDataOrDoctypeStarted => self.comment_or_cdata_or_doctype_started(c),
            CommentStarted                 => self.comment_started(c),
            CDataStarted(s)                => self.cdata_started(c, s),
            DoctypeStarted(s)              => self.doctype_started(c, s),
            ProcessingInstructionClosing   => self.processing_instruction_closing(c),
            EmptyTagClosing                => self.empty_element_closing(c),
            CommentClosing(s)              => self.comment_closing(c, s),
            CDataClosing(s)                => self.cdata_closing(c, s)
        }
    }

    #[inline]
    fn move_to(&mut self, st: State) -> LexStep {
        self.st = st;
        None
    }

    #[inline]
    fn move_to_with(&mut self, st: State, token: Token) -> LexStep {
        self.st = st;
        Some(Ok(token))
    }
    
    #[inline]
    fn move_to_with_unread(&mut self, st: State, c: char, token: Token) -> LexStep {
        self.temp_char = Some(c);
        self.move_to_with(st, token)
    }

    fn handle_error(&mut self, chunk: ~str, c: char) -> LexStep {
        self.temp_char = Some(c);
        if self.skip_errors {
            self.move_to_with(Normal, Chunk(chunk))
        } else {
            Some(Err(
                Error::new_full(
                    self.row, self.col-chunk.len()-1,
                    format!("Unexpected token {} before {}", chunk, c)
                )
            ))
        }
    }

    /// Encountered a char
    fn normal(&mut self, c: char) -> LexStep {
        match c {
            '<'                        => self.move_to(TagOpened),
            '>'                        => Some(Ok(TagEnd)),
            '/'                        => self.move_to(EmptyTagClosing),
            '='                        => Some(Ok(EqualsSign)),
            '"'                        => Some(Ok(DoubleQuote)),
            '\''                       => Some(Ok(SingleQuote)),
            '?'                        => self.move_to(ProcessingInstructionClosing),
            '-'                        => self.move_to(CommentClosing(First)),
            ']'                        => self.move_to(CDataClosing(First)),
            '&'                        => Some(Ok(ReferenceStart)),
            ';'                        => Some(Ok(ReferenceEnd)),
            _ if is_whitespace_char(c) => Some(Ok(Whitespace(c))),
            _                          => Some(Ok(Character(c)))
        }
    }

    /// Encountered '<'
    fn tag_opened(&mut self, c: char) -> LexStep {
        match c {
            '?'                        => self.move_to_with(Normal, ProcessingInstructionStart),
            '/'                        => self.move_to_with(Normal, ClosingTagStart),
            '!'                        => self.move_to(CommentOrCDataOrDoctypeStarted),
            _ if is_whitespace_char(c) => self.move_to_with_unread(Normal, c, OpeningTagStart),
            _ if is_name_char(c)       => self.move_to_with_unread(Normal, c, OpeningTagStart),
            _                          => self.handle_error(~"<", c)
        }
    }

    /// Encountered '<!'
    fn comment_or_cdata_or_doctype_started(&mut self, c: char) -> LexStep {
        match c {
            '-' => self.move_to(CommentStarted),
            '[' => self.move_to(CDataStarted(E)),
            'D' => self.move_to(DoctypeStarted(D)),
            _   => self.handle_error(~"<!", c)
        }
    }

    /// Encountered '<!-'
    fn comment_started(&mut self, c: char) -> LexStep {
        match c {
            '-' => self.move_to_with(Normal, CommentStart),
            _   => self.handle_error(~"<!-", c)
        }
    }

    /// Encountered '<!['
    fn cdata_started(&mut self, c: char, s: CDataStartedSubstate) -> LexStep {
        dispatch_on_enum_state!(s, c, CDataStarted,
            E     -> 'C' -> C     ! "<![",
            C     -> 'D' -> CD    ! "<![C",
            CD    -> 'A' -> CDA   ! "<![CD",
            CDA   -> 'T' -> CDAT  ! "<![CDA",
            CDAT  -> 'A' -> CDATA ! "<![CDAT";
            CDATA -> '[' ! "<![CDATA" -> self.move_to_with(Normal, CDataStart) 
        )
    }

    /// Encountered '<!D'
    fn doctype_started(&mut self, c: char, s: DoctypeStartedSubstate) -> LexStep {
        dispatch_on_enum_state(s, c, DoctypeStarted,
            D      -> 'O' -> DO      ! "<!D",
            DO     -> 'C' -> DOC     ! "<!DO",
            DOC    -> 'T' -> DOCT    ! "<!DOC",
            DOCT   -> 'Y' -> DOCTY   ! "<!DOCT",
            DOCTY  -> 'P' -> DOCTYP  ! "<!DOCTY";
            DOCTYP -> 'E' ! "<!DOCTYP" -> self.move_to_with(Normal, DoctypeStart)
    }

    /// Encountered '?'
    fn processing_instruction_closing(&mut self, c: char) -> LexStep {
        match c {
            '>' => self.move_to_with(Normal, ProcessingInstructionEnd),
            _   => self.move_to_with_unread(Normal, c, Character('?')),
        }
    }

    /// Encountered '/'
    fn empty_element_closing(&mut self, c: char) -> LexStep {
        match c {
            '>' => self.move_to_with(Normal, EmptyTagEnd),
            _   => self.move_to_with_unread(Normal, c, Character('/')),
        }
    }

    /// Encountered '-'
    fn comment_closing(&mut self, c: char, s: ClosingSubstate) -> LexStep {
        match s {
            First => match c {
                '-' => self.move_to(CommentClosing(Second)),
                _   => self.move_to_with_unread(Normal, c, Character('-'))
            },
            Second => match c {
                '>' => self.move_to_with(Normal, CommentEnd),
                _   => self.move_to_with_unread(Normal, c, Chunk(~"--"))
            }
        }
    }

    /// Encountered ']'
    fn cdata_closing(&mut self, c: char, s: ClosingSubstate) -> LexStep {
        match s {
            First => match c {
                ']' => self.move_to(CDataClosing(Second)),
                _   => self.move_to_with_unread(Normal, c, Character(']'))
            },
            Second => match c {
                '>' => self.move_to_with(Normal, CDataEnd),
                _   => self.move_to_with_unread(Normal, c, Chunk(~"]]"))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::mem::MemReader;

    use common::{HasPosition};

    use super::{
        PullLexer,
        ProcessingInstructionStart,
        ProcessingInstructionEnd,
        OpeningTagStart,
        ClosingTagStart,
        TagEnd,
        EmptyTagEnd,
        CommentStart,
        CommentEnd,
        Chunk,
        Character,
        Whitespace,
        CDataStart,
        CDataEnd,
        ReferenceStart,
        ReferenceEnd,
        SingleQuote,
        DoubleQuote,
        EqualsSign
    };

    macro_rules! assert_oks(
        (for $lex:ident and $buf:ident $($e:expr)+) => ({
            $(
                assert_eq!(Some(Ok($e)), $lex.next_token(&mut $buf));
             )+
        })
    )

    macro_rules! assert_err(
        (for $lex:ident and $buf:ident expect row $r:expr col $c:expr, $s:expr) => ({
            let err = $lex.next_token(&mut $buf);
            assert!(err.is_some());
            assert!(err.get_ref().is_err());
            let err = err.unwrap().unwrap_err();
            assert_eq!($r as uint, err.row());
            assert_eq!($c as uint, err.col());
            assert_eq!($s, err.msg());
        })
    )

    macro_rules! assert_none(
        (for $lex:ident and $buf:ident) => (
            assert_eq!(None, $lex.next_token(&mut $buf))
        )
    )

    fn make_buf(s: ~str) -> MemReader {
        MemReader::new(s.into_bytes())
    }

    fn make_lex_and_buf(s: ~str) -> (PullLexer, MemReader) {
        (super::new(), make_buf(s))
    }

    #[test]
    fn simple_lexer_test() {
        let (mut lex, mut buf) = make_lex_and_buf(
            ~r#"<a p='q'> x<b z="y">d	</b></a><p/> <?nm ?> <!-- a c --> &nbsp;"#
        );

        assert_oks!(for lex and buf
            OpeningTagStart
            Character('a')
            Whitespace(' ')
            Character('p')
            EqualsSign
            SingleQuote
            Character('q')
            SingleQuote
            TagEnd
            Whitespace(' ')
            Character('x')
            OpeningTagStart
            Character('b')
            Whitespace(' ')
            Character('z')
            EqualsSign
            DoubleQuote
            Character('y')
            DoubleQuote
            TagEnd
            Character('d')
            Whitespace('\t')
            ClosingTagStart
            Character('b')
            TagEnd
            ClosingTagStart
            Character('a')
            TagEnd
            OpeningTagStart
            Character('p')
            EmptyTagEnd
            Whitespace(' ')
            ProcessingInstructionStart
            Character('n')
            Character('m')
            Whitespace(' ')
            ProcessingInstructionEnd
            Whitespace(' ')
            CommentStart
            Whitespace(' ')
            Character('a')
            Whitespace(' ')
            Character('c')
            Whitespace(' ')
            CommentEnd
            Whitespace(' ')
            ReferenceStart
            Character('n')
            Character('b')
            Character('s')
            Character('p')
            ReferenceEnd
        )
        assert_none!(for lex and buf);
    }

    #[test]
    fn special_chars_test() {
        let (mut lex, mut buf) = make_lex_and_buf(
            ~r#"?x!+ // -| ]z]]"#
        );

        assert_oks!(for lex and buf
            Character('?')
            Character('x')
            Character('!')
            Character('+')
            Whitespace(' ')
            Character('/')
            Character('/')
            Whitespace(' ')
            Character('-')
            Character('|')
            Whitespace(' ')
            Character(']')
            Character('z')
            Chunk(~"]]")
        )
        assert_none!(for lex and buf);
    }

    #[test]
    fn cdata_test() {
        let (mut lex, mut buf) = make_lex_and_buf(
            ~r#"<a><![CDATA[x y ?]]> </a>"#
        );

        assert_oks!(for lex and buf
            OpeningTagStart
            Character('a')
            TagEnd
            CDataStart
            Character('x')
            Whitespace(' ')
            Character('y')
            Whitespace(' ')
            Character('?')
            CDataEnd
            Whitespace(' ')
            ClosingTagStart
            Character('a')
            TagEnd
        )
        assert_none!(for lex and buf);
    }

    #[test]
    fn end_of_stream_handling_ok() {
        macro_rules! eof_check(
            ($data:expr -> $token:expr) => ({
                let (mut lex, mut buf) = make_lex_and_buf(~$data);
                assert_oks!(for lex and buf $token);
                assert_none!(for lex and buf);
            })
        )
        eof_check!("?"  -> Character('?'));
        eof_check!("/"  -> Character('/'));
        eof_check!("-"  -> Character('-'));
        eof_check!("]"  -> Character(']'));
        eof_check!("]]" -> Chunk(~"]]"));
    }

    #[test]
    fn end_of_stream_handling_error() {
        macro_rules! eof_check(
            ($data:expr -> $r:expr, $c:expr) => ({
                let (mut lex, mut buf) = make_lex_and_buf(~$data);
                assert_err!(for lex and buf expect row $r col $c, "Unexpected end of stream");
                assert_none!(for lex and buf); 
            })
        )
        eof_check!("<"        -> 0, 1);
        eof_check!("<!"       -> 0, 2);
        eof_check!("<!-"      -> 0, 3);
        eof_check!("<!["      -> 0, 3);
        eof_check!("<![C"     -> 0, 4);
        eof_check!("<![CD"    -> 0, 5);
        eof_check!("<![CDA"   -> 0, 6);
        eof_check!("<![CDAT"  -> 0, 7);
        eof_check!("<![CDATA" -> 0, 8);
        eof_check!("--"       -> 0, 2);
    }

    #[test]
    fn error_in_comment_or_cdata_prefix() {
        let (mut lex, mut buf) = make_lex_and_buf(~"<!x");
        assert_err!(for lex and buf expect row 0 col 0,
            "Unexpected token <! before x"
        );

        let (mut lex, mut buf) = make_lex_and_buf(~"<!x");
        lex.disable_errors();
        assert_oks!(for lex and buf 
            Chunk(~"<!")
            Character('x')
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn error_in_comment_started() {
        let (mut lex, mut buf) = make_lex_and_buf(~"<!-\t");
        assert_err!(for lex and buf expect row 0 col 0,
            "Unexpected token <!- before \t"
        );

        let (mut lex, mut buf) = make_lex_and_buf(~"<!-\t");
        lex.disable_errors();
        assert_oks!(for lex and buf 
            Chunk(~"<!-")
            Whitespace('\t')
        );
        assert_none!(for lex and buf);
    }

    #[test]
    fn error_in_cdata_started() {
        macro_rules! cdata_case(
            ($chunk:expr, $app:expr; $data:expr -> $r:expr, $c:expr, $s:expr) => ({
                let (mut lex, mut buf) = make_lex_and_buf(~$data);
                assert_err!(for lex and buf expect row $r col $c, $s);

                let (mut lex, mut buf) = make_lex_and_buf(~$data);
                lex.disable_errors();
                assert_oks!(for lex and buf 
                    Chunk(~$chunk)
                    Character($app)
                );
                assert_none!(for lex and buf);
            })
        )
        cdata_case!("<![",      '['; "<![["      -> 0, 0, "Unexpected token <![ before [");
        cdata_case!("<![C",     '['; "<![C["     -> 0, 0, "Unexpected token <![C before [");
        cdata_case!("<![CD",    '['; "<![CD["    -> 0, 0, "Unexpected token <![CD before [");
        cdata_case!("<![CDA",   '['; "<![CDA["   -> 0, 0, "Unexpected token <![CDA before [");
        cdata_case!("<![CDAT",  '['; "<![CDAT["  -> 0, 0, "Unexpected token <![CDAT before [");
        cdata_case!("<![CDATA", '|'; "<![CDATA|" -> 0, 0, "Unexpected token <![CDATA before |");
    }

    #[test]
    fn error_in_comment_closing() {
        let (mut lex, mut buf) = make_lex_and_buf(~"--+");
        assert_err!(for lex and buf expect row 0 col 0,
            "Unexpected token -- before +"
        );

        let (mut lex, mut buf) = make_lex_and_buf(~"--+");
        lex.disable_errors();
        assert_oks!(for lex and buf
            Chunk(~"--")
            Character('+')
        );
        assert_none!(for lex and buf);
    }

    // TODO: line counting
}
