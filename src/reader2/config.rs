use std::io::Read;

use reader2::Parser;

pub struct ParserConfig {
    pub trim_whitespace: bool,

    pub whitespace_to_characters: bool,

    pub cdata_to_characters: bool,

    pub coalesce_characters: bool,

    pub ignore_comments: bool,
}

impl ParserConfig {
    pub fn new() -> ParserConfig {
        ParserConfig {
            trim_whitespace: false,
            whitespace_to_characters: false,
            cdata_to_characters: false,
            coalesce_characters: true,
            ignore_comments: true,
        }
    }

    pub fn create_parser<R: Read>(self, source: R) -> Parser<R> {
        Parser::new(self, source)
    }
}

gen_setters! { ParserConfig,
    trim_whitespace: val bool,
    whitespace_to_characters: val bool,
    cdata_to_characters: val bool,
    coalesce_characters: val bool,
    ignore_comments: val bool
}
