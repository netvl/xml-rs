//! Contains emitter configuration structure.
use std::io::Write;

use writer::EventWriter;

/// Emitter configuration structure.
///
/// This structure contains various options which control XML document emitter behavior.
#[derive(Clone, PartialEq, Eq, Debug)]
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

    /// Whether or not characters in output events will be escaped. Default is true.
    ///
    /// The emitter can automatically escape characters which can't appear in PCDATA sections
    /// or element attributes of an XML document, like `<` or `"` (in attributes). This may
    /// introduce some overhead because then every corresponding piece of character data
    /// should be scanned for invalid characters.
    ///
    /// If this option is disabled, the XML writer may produce non-well-formed documents, so
    /// use `false` value for this option with care.
    pub perform_escaping: bool,

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
    pub cdata_to_characters: bool,

    /// Whether or not to keep element names to support `EndElement` events without explicit names.
    /// Default is true.
    ///
    /// This option makes the emitter to keep names of written elements in order to allow
    /// omitting names when writing closing element tags. This could incur some memory overhead.
    pub keep_element_names_stack: bool
}

impl EmitterConfig {
    /// Creates an emitter configuration with default values.
    ///
    /// You can tweak default options with builder-like pattern:
    /// ```rust
    /// let config = EmitterConfig::new()
    ///     .line_separator("\r\n".into())
    ///     .perform_indent(true)
    ///     .normalize_empty_elements(false);
    /// ```
    pub fn new() -> EmitterConfig {
        EmitterConfig {
            line_separator: "\n".into(),
            indent_string: "  ".into(),  // two spaces
            perform_indent: false,
            perform_escaping: true,
            write_document_declaration: true,
            normalize_empty_elements: true,
            cdata_to_characters: false,
            keep_element_names_stack: true
        }
    }

    #[inline]
    pub fn create_writer<W: Write>(self, sink: W) -> EventWriter<W> {
        EventWriter::new_with_config(sink, self)
    }
}

impl Default for EmitterConfig {
    #[inline]
    fn default() -> EmitterConfig {
        EmitterConfig::new()
    }
}

gen_setters!(EmitterConfig,
    line_separator: String,
    indent_string: String,
    perform_indent: bool,
    write_document_declaration: bool,
    normalize_empty_elements: bool,
    cdata_to_characters: bool,
    keep_element_names_stack: bool
);
