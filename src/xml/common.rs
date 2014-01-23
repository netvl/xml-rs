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

pub struct Error {
    line: uint,
    col: uint,
    msg: ~str
}

pub fn is_whitespace(c: char) -> bool {
    match c {
        '\x20'|'\x09'|'\x0d'|'\x0a' => true,
        _ => false
    }
}

pub fn is_name_char(c: char) -> bool {
    // TODO: perform actual name characters checking
    !is_whitespace(c)
}
