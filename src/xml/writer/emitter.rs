//!
//!

use common;
use common::{Error, XmlVersion, Name, NamespaceStack, is_name_start_char, is_name_char, is_whitespace_char};
use events;
use events::XmlEvent;

use writer::config::EmitterConfig;

pub struct Emitter {
    priv config: EmitterConfig,
    priv indent_level: uint,
    priv indent_stack: ~[u8]
}

pub fn new(config: EmitterConfig) -> Emitter {
    Emitter {
        config: config,
        indent_level: 0,
        indent_stack: ~[]
    }
}

impl Emitter {
    pub fn emit(target: Writer, event: XmlEvent) {
        
    }
}
