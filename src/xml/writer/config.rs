
pub struct EmitterConfig {
    pub line_separator: ~str,
    pub indent_string: ~str,
    pub perform_indent: bool,
    pub write_document_declaration: bool,
    pub normalize_empty_elements: bool,
    pub cdata_to_characters: bool
}

impl EmitterConfig {
    pub fn new() -> EmitterConfig {
        EmitterConfig {
            line_separator: box "\n",
            indent_string: box "  ",  // two spaces
            write_document_declaration: true,
            perform_indent: false,
            normalize_empty_elements: true,
            cdata_to_characters: false
        }
    }
}

gen_setters!(EmitterConfig,
    line_separator: ~str,
    indent_string: ~str,
    perform_indent: bool,
    write_document_declaration: bool,
    normalize_empty_elements: bool,
    cdata_to_characters: bool
)
