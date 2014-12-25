//! Contains emitter configuration structure.

use std::borrow::ToOwned;

/// Emitter configuration structure.
///
/// This structure contains various options which control XML document emitter behavior.
pub struct EmitterConfig {
    /// Line separator used to separate lines in formatted output. Default is `"\n"`.
    pub line_separator: String,

    /// A string which will be used for a single level of indentation. Default is `"  "`
    /// (two spaces).
    pub indent_string: String,

    /// Whether or not the emitted document should be indented. Default is false.
    ///
    /// The emitter is capable to perform automatic indentation of the emitted XML document.
    /// It is done in stream-like fashion and does not require the knowledge of the whole
    /// document in advance.
    ///
    /// Sometimes, however, automatic indentation is undesirable, e.g. when you want to keep
    /// existing layout when processing an existing XML document. Also the indentiation algorithm
    /// is not thoroughly tested. Hence by default it is disabled.
    pub perform_indent: bool,

    /// Whether or not to write XML document declaration at the beginning of a document.
    /// Default is true.
    ///
    /// This option controls whether the document declaration should be emitted automatically
    /// before a root element is written if it was not emitted explicitly by the user.
    pub write_document_declaration: bool,

    /// Whether or not to convert elements with empty content to empty elements. Default is true.
    ///
    /// This option allows turning elements like `<a></a>` (an element with empty content)
    /// into `<a />` (an empty element).
    pub normalize_empty_elements: bool,

    /// Whether or not to emit CDATA events as plain characters. Default is false.
    ///
    /// This option forces the emitter to convert CDATA events into regular character events,
    /// performing all the necessary escaping beforehand. This may be occasionally useful
    /// for feeding the document into incorrect parsers which do not support CDATA.
    pub cdata_to_characters: bool
}

impl EmitterConfig {
    /// Creates an emitter configuration with default values.
    ///
    /// You can tweak default options with builder-like pattern:
    /// ```rust
    /// let config = EmitterConfig::new()
    ///     .line_separator("\r\n".to_owned())
    ///     .perform_indent(true)
    ///     .normalize_empty_elements(false);
    /// ```
    pub fn new() -> EmitterConfig {
        EmitterConfig {
            line_separator: "\n".to_owned(),
            indent_string: "  ".to_owned(),  // two spaces
            perform_indent: false,
            write_document_declaration: true,
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
);
