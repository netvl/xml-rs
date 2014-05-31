
pub struct EmitterConfig {
    pub line_separator: String,
    pub indent_string: String,
    pub perform_indent: bool,
    pub write_document_declaration: bool,
    pub normalize_empty_elements: bool,
    pub cdata_to_characters: bool
}

impl EmitterConfig {
    pub fn new() -> EmitterConfig {
        EmitterConfig {
            line_separator: "\n".to_string(),
            indent_string: "  ".to_string(),  // two spaces
            write_document_declaration: true,
            perform_indent: false,
            normalize_empty_elements: true,
            cdata_to_characters: false
        }
    }
}

gen_setters!(EmitterConfig,
    line_separator: String,
    indent_string: String,
    perform_indent: bool,
    write_document_declaration: bool,
    normalize_empty_elements: bool,
    cdata_to_characters: bool
)
