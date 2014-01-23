use std::util;

use common::{is_whitespace, is_name_char};

pub enum Token {
    ProcessingInstructionStart,
    ProcessingInstructionEnd,
    OpenElementStart,
    ClosingElementStart,
    ElementEnd,
    CommentStart,
    CommentEnd,
    Chunk(~str),
    Char(char),
    Whitespace(char),
    CDataStart,
    CDataEnd,
    ReferenceStart,
    ReferenceEnd,
}

impl ToStr for Token {
    fn to_str(&self) -> ~str {
        match *self {
            Chunk(s)      => s,
            Char(c)       => c.to_str(),
            Whitespace(c) => c.to_str(),
            _ => ~match *self {
                OpenElementStart           => "<",
                ProcessingInstructionStart => "<?",
                ClosingElementStart        => "</",
                CommentStart               => "<!--",
                CDataStart                 => "<![CDATA[",
                ElementEnd                 => ">",
                ProcessingInstructionEnd   => "?>",
                CommentEnd                 => "-->",
                CDataEnd                   => "]]>",
                ReferenceStart             => "&",
                ReferenceEnd               => ";",
                _                          => ureachable!()
            }
        }
        
    }
}

enum State {
    /// Triggered on '<'
    TagOpened,
    /// Triggered on '<!'
    CommentOrCDataStarted,
    /// Triggered on '<!-'
    CommentStarted,
    /// Triggered when '<![' up to '<![CDATA'
    CDataStarted(CDataStartedSubstate),
    /// Triggered on '?'
    ProcessingInstructionClosing,
    /// Triggered when '-' and '--'
    CommentClosing(CloseSubstate),
    /// Triggered when ']' and ']]'
    CDataClosing(CloseSubstate),
    /// Default state
    Normal
}

enum ClosingSubstate {
    First, Second
}

enum CDataStartedSubstate {
    E, C, CD, CDA, CDAT, CDATA
}

type LexResult = Result<Token, ~str>;
type LexStep = Option<LexResult>;  // TODO: better name

macro_rules! self_error(
    ($msg:expr, $($arg),*) => (
        self.error(format!($msg, $($arg),*))
    )
)

pub struct PullLexer {
    priv row: uint,
    priv col: uint,
    priv temp_token: Option<Token>,
    priv temp_char: Option<char>,
    priv st: State,
    priv skip_errors: bool
}

pub fn new() -> PullLexer {
    PullLexer {
        row: 0,
        col: 0,
        temp_token: None,
        temp_char: None,
        st: Normal,
        skip_error: false
    }
}

impl PullLexer {
    pub fn next_token<B: Buffer>(b: &mut B) -> LexResult {
        if temp_token.is_some() {
            return Ok(util::replace(&mut self.temp_token, None).unwrap());
        }

        if temp_char.is_some() {
            match self.read_next_token(util::replace(&mut self.temp_char, None).unwrap()) {
                Some(t) => return t,
                None => {}  // continue
            }
        }

        foreach!(c in b.read_char() {
            match self.read_next_token(c) {
                Some(t) => return t,
                None    => {}  // continue
            }
        })

        Err(self.error("Unexpected end of stream"))
    }

    fn error(msg: ~str) -> ~str {
        format!("Line {}, column {}: {}", self.row + 1, self.col + 1, msg)
    }

    fn read_next_token(c: char) -> LexStep {
        if c == '\n' {
            self.row += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }

        self.dispatch_char(c)
    }

    #[inline]
    pub fn row() -> uint { self.row }

    #[inline]
    pub fn col() -> uint { self.col }

    fn dispatch_char(&mut self, c: char) -> LexStep {
        match self.st {
            Normal                       => self.normal(c),
            TagOpened                    => self.tag_opened(c),
            CommentOrCDataStarted        => self.comment_or_cdata_started(c),
            CommentStarted               => self.comment_started(c),
            CDataStarted(s)              => self.cdata_started(c, s),
            ProcessingInstructionClosing => self.processing_instruction_closing(c),
            CommentClosing(s)            => self.comment_closing(c, s),
            CDataClosing(s)              => self.cdata_closing(c, s)
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
        Some(token)
    }
    
    #[inline]
    fn move_to_with_temp(&mut self, st: State, temp_token: Token, token: Token) -> LexStep {
        self.temp_token = Some(temp_token);
        self.move_to_with(st, token)
    }

    #[inline]
    fn chunk(&mut self, mut prefix: ~str, c: char) -> ~str {
        prefix.push(c);
        prefix
    }

    fn handle_error(&mut self, chunk: ~str, c: char) -> LexStep {
        if self.skip_errors {
            self.move_to_with(Normal, self.chunk(chunk, c))
        } else {
            self.temp_char = Some(c);
            Some(Err(self_error!("Unexpected token: {}", chunk)))
        }
    }

    /// Encountered a char
    fn normal(&mut self, c: char) -> LexStep {
        match c {
            '<'                   => self.move_to(TagOpened),
            '?'                   => self.move_to(ProcessingInstructionClosing),
            '-'                   => self.move_to(CommentClosing(First)),
            ']'                   => self.move_to(CDataClosing(Second)),
            '&'                   => Some(ReferenceStart),
            ';'                   => Some(ReferenceEnd),
            _ if is_whitespace(c) => Some(Whitespace(c)),
            _                     => Some(Char(c))
        }
    }

    /// Encountered '<'
    fn tag_opened(&mut self, c: char) -> LexStep {
        match c {
            '?'                   => self.move_to_with(Normal, ProcessingInstructionStart),
            '/'                   => self.move_to_with(Normal, ClosingElementStart),
            '!'                   => self.move_to(CommentOrCDataStarted),
            _ if is_whitespace(c) => {
                self.temp_token = Whitespace(c),
                self.move_to_with(Normal, OpenElementStart)
            }
            _                     => self.handle_error(~"<", c)
        }
    }

    /// Encountered '<!'
    fn comment_or_cdata_started(&mut self, c: char) -> LexStep {
        match c {
            '-' => self.move_to(CommentStarted),
            '[' => self.move_to(CDataStarted(E)),
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
    fn cdata_started(&mut self, c: char, s: CDataStartSubstate) -> LexStep {
        match s {
            E => match c {
                'C' => self.move_to(CDataStarted(C)),
                _   => self.handle_error(~"<![", c)
            }
            C => match c {
                'D' => self.move_to(CDataStarted(CD)),
                _   => self.handle_error(~"<![C", c)
            }
            CD => match c {
                'A' => self.move_to(CDataStarted(CDA)),
                _   => self.handle_error(~"<![CD", c)
            }
            CDA => match c {
                'T' => self.move_to(CDataStarted(CDAT)),
                _   => self.handle_error(~"<![CDA", c)
            }
            CDAT => match c {
                'A' => self.move_to(CDataStarted(CDATA)),
                _   => self.handle_error(~"<![CDAT", c)
            }
            CDATA => match c {
                '[' => self.move_to_with(Normal, CDataStart),
                _   => self.handle_error(~"<![CDATA", c)
            }
        }
    }

    /// Encountered '?'
    fn processing_instruction_closing(&mut self, c: char) -> LexStep {
        match c {
            '>' => self.move_to_with(Normal, ProcessingInstructionEnd),
            _   => self.handle_error(~"?", c),
        }
    }

    /// Encountered '-'
    fn comment_closing(&mut self, c: char, s: ClosingSubstate) -> LexSep {
        match n {
            First => match c {
                '-'                   => self.move_to(CommentClosing(Second)),
                _ if is_whitespace(c) => self.move_to_with_temp(Normal, Whitespace(c), Char('-')),
                _                     => self.move_to_with_temp(Normal, Char(c), Char('-'))
            }
            Second => match c {
                '>' => self.move_to_with(Normal, CommentEnd),
                _   => self.handle_error(~"--", c)
            }
        }
    }

    /// Encountered ']'
    fn cdata_closing(&mut self, c: char, s: ClosingSubstate) -> LexSep {
        match n {
            First => match c {
                ']'                   => self.move_to(CDataClosing(Second)),
                _ if is_whitespace(c) => self.move_to_with_temp(Normal, Whitespace(c), Char(']')),
                _                     => self.move_to_with_temp(Normal, Char(c), Char(']'))
            }
            Second => match c {
                '>'                   => self.move_to(CDataEnd),
                _ if is_whitespace(c) => self.move_to_with_temp(Normal, Whitespace(c), Chunk("]]")),
                _                     => self.move_to_with_temp(Normal, Char(c), Chunk("]]"))
            }
        }
    }
}

