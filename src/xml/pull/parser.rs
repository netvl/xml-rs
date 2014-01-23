use events::{XmlEvent, EndDocument, Error};

pub struct PullParser {
    priv row: uint,
    priv col: uint,
    priv st: State,
    priv buf: ~[char],

    priv inside_whitespace: bool
}

pub fn new() -> PullParser {
    PullParser {
        row: 0,
        col: 0,
        st: OutsideTag,
        buf: ~[],

        inside_whitespace: true
    }
}

enum State {
    OutsideTag,
    TagStarted,
    InsideOpeningTag,
    InsideClosingTag
}

macro_rules! foreach(
    ($c:ident in $r:ident $b:expr) => (
        loop {
            match $r.read_char() {
                Some($c) => $b,
                None => {}
            }
        }
    )
)

impl PullParser {
    pub fn next<B: Buffer>(&mut self, r: &mut B) -> XmlEvent {
        foreach!(c in r {
            if c == '\n' {
                self.row += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }

            match self.parse_char(c) {
                Some(ev) => return ev,
                None => {}  // continue
            }
        })
        self.error(~"Unexpected end of stream")
    }

    fn error(&self, msg: ~str) -> XmlEvent {
        Error { row: self.row+1, col: self.col+1, msg: msg }
    }

    fn parse_char(&mut self, c: char) -> Option<XmlEvent> {
        match self.st {
            OutsideTag => self.outside_tag(c)
        }
    }

    fn outside_tag(&mut self, c: char) -> Option<XmlEvent> {
        match c {
            '<' if self.buf.len() > 0 => {
                self.st = InsideTag;
                let buf = self.buf.clone();
                self.buf.clear();
                if self.inside_whitespace {
                    Whitespace(buf)
                } else {
                    Characters(buf)
                }
            }
            '<' => {
                self.st = InsideTag;
                None
            },
            _ => self.buf.push(c)
        }
    }

}
