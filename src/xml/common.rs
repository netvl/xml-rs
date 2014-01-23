pub struct Name {
    prefix: Option<~str>,
    namespace: Option<~str>,
    local_name: ~str
}

pub struct Attribute {
    name: Name,
    value: ~str
}

pub enum XmlVersion {
    VERSION_1_0,
    VERSION_1_1
}

#[deriving(Eq)]
pub struct Error {
    priv row: uint,
    priv col: uint,
    priv msg: ~str
}

pub trait HasPosition {
    fn row(&self) -> uint;
    fn col(&self) -> uint;
}

impl ToStr for Error {
    #[inline]
    fn to_str(&self) -> ~str {
        format!("{}:{}: {}", self.row + 1, self.col + 1, self.msg)
    }
}

impl Error {
    #[inline]
    pub fn new<O: HasPosition>(o: &O, msg: ~str) -> Error {
        Error { row: o.row(), col: o.col(), msg: msg }
    }

    #[inline]
    pub fn new_full(row: uint, col: uint, msg: ~str) -> Error {
        Error { row: row, col: col, msg: msg }
    }

    #[inline]
    pub fn row(&self) -> uint { self.row }

    #[inline]
    pub fn col(&self) -> uint { self.col }

    #[inline]
    pub fn msg<'a>(&'a self) -> &'a str { self.msg.as_slice() }
}

pub fn is_whitespace(c: char) -> bool {
    match c {
        '\x20'|'\x09'|'\x0d'|'\x0a' => true,
        _ => false
    }
}

#[inline]
pub fn is_name_char(c: char) -> bool {
    // TODO: perform actual name characters checking
    !is_whitespace(c)
}
