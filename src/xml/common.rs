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
