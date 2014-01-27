pub struct ParserConfig {
    trim_whitespace: bool,
    whitespace_to_characters: bool,
    cdata_to_characters: bool,
    ignore_comments: bool
}

impl ParserConfig {
    pub fn new() -> ParserConfig {
        ParserConfig {
            trim_whitespace: false,
            whitespace_to_characters: false,
            cdata_to_characters: false,
            ignore_comments: false
        }
    }
}

macro_rules! gen_setters(
    ($target:ty, $($field:ident : $t:ty),+) => ($(
        impl $target {
            pub fn $field<'a>(&'a mut self, value: $t) -> &'a mut $target {
                self.$field = value;
                self
            }
        }
    )+)
)

gen_setters!(ParserConfig,
    trim_whitespace : bool,
    whitespace_to_characters: bool,
    cdata_to_characters: bool,
    ignore_comments: bool
)
