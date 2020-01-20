//! Contains the writer configuration structure.

use std::borrow::Cow;
use std::io::Write;

use super::Writer;

/// Writer configuration structure.
///
/// This structure contains various options which control behavior of an XML document writer.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct WriterConfig {
    /// Line separator used to separate lines in formatted output. Default is `"\n"`.
    pub line_separator: Cow<'static, str>,

    /// A string which will be used for a single level of indentation. Default is `"  "`
    /// (two spaces).
    pub indent_string: Cow<'static, str>,

    /// Whether or not the emitted document should be indented. Default is false.
    ///
    /// The writer is capable to perform automatic indentation of the emitted XML document.
    /// It is done in stream-like fashion and does not require the knowledge of the whole
    /// document in advance.
    ///
    /// Sometimes, however, automatic indentation is undesirable, e.g. when you want to keep
    /// existing layout when processing an existing XML document. Also the indentiation algorithm
    /// is not thoroughly tested. Hence by default it is disabled.
    pub perform_indent: bool,

    /// Whether or not characters in output events will be escaped. Default is true.
    ///
    /// The writer can automatically escape characters which can't appear in PCDATA sections
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
    /// This option forces the writer to convert CDATA events into regular text events,
    /// performing all the necessary escaping beforehand. This may be occasionally useful
    /// for feeding the document into incorrect parsers which do not support CDATA.
    pub cdata_to_text: bool,

    /// Whether or not to keep element names to support `EndElement` events without explicit names.
    /// Default is true.
    ///
    /// This option makes the writer to keep names of written elements in order to allow
    /// omitting names when writing closing element tags. This could incur some memory overhead.
    pub keep_element_names_stack: bool,

    /// Whether or not to automatically insert leading and trailing spaces in emitted comments,
    /// if necessary. Default is true.
    ///
    /// This is a convenience option in order for the user not to append spaces before and after
    /// comments text in order to get more pretty comments: `<!-- something -->` instead of
    /// `<!--something-->`.
    pub autopad_comments: bool,
}

impl WriterConfig {
    /// Creates a writer configuration with default values.
    ///
    /// You can tweak default options with builder-like pattern:
    ///
    /// ```rust
    /// use xml::writer::WriterConfig;
    ///
    /// let config = WriterConfig::new()
    ///     .line_separator("\r\n")
    ///     .perform_indent(true)
    ///     .normalize_empty_elements(false);
    /// ```
    #[inline]
    pub fn new() -> WriterConfig {
        WriterConfig {
            line_separator: "\n".into(),
            indent_string: "  ".into(), // two spaces
            perform_indent: false,
            perform_escaping: true,
            write_document_declaration: true,
            normalize_empty_elements: true,
            cdata_to_text: false,
            keep_element_names_stack: true,
            autopad_comments: true,
        }
    }

    /// Creates an XML writer with this configuration.
    ///
    /// This is a convenience method for configuring and creating a writer at the same time:
    ///
    /// ```rust
    /// use xml::writer::WriterConfig;
    ///
    /// let mut target: Vec<u8> = Vec::new();
    ///
    /// let writer = WriterConfig::new()
    ///     .line_separator("\r\n")
    ///     .perform_indent(true)
    ///     .normalize_empty_elements(false)
    ///     .create_writer(&mut target);
    /// ```
    ///
    /// This method is exactly equivalent to calling `EventWriter::new_with_config()` with
    /// this configuration object.
    pub fn create_writer<W: Write>(self, sink: W) -> Writer<W> {
        Writer::new_with_config(sink, self)
    }
}

impl Default for WriterConfig {
    fn default() -> WriterConfig {
        WriterConfig::new()
    }
}

gen_setters!(WriterConfig,
    line_separator: into Cow<'static, str>,
    indent_string: into Cow<'static, str>,
    perform_indent: val bool,
    write_document_declaration: val bool,
    normalize_empty_elements: val bool,
    cdata_to_text: val bool,
    keep_element_names_stack: val bool,
    autopad_comments: val bool
);
