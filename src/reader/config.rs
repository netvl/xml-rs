use std::collections::HashMap;
use std::io::BufRead;

use crate::reader::data::{DecodingReader, StrRead};
use crate::reader::Reader;

/// Reader configuration structure.
///
/// This structure contains various configuration options which affect behavior of the reader.
#[derive(Clone, Debug)]
pub struct ReaderConfig {
    /// Whether or not whitespace in textual events should be removed. Default is false.
    ///
    /// When true, all standalone whitespace will be removed (this means no
    /// `Whitespace` events will be emitted), and leading and trailing whitespace
    /// from `Text` events will be deleted. If after trimming a `Text` event is empty, it will also
    /// be omitted from output stream. This is possible, however, only if `whitespace_to_text` or
    /// `cdata_to_text` options are set.
    ///
    /// This option does not affect CDATA events, unless `cdata_to_text`
    /// option is also set. In that case CDATA content will also be trimmed.
    pub trim_whitespace: bool,

    /// Whether or not whitespace should be converted to text. Default is false.
    ///
    /// If true, instead of `Whitespace` events, `Text` events with the same content will be emitted.
    ///If `trim_whitespace` is also true, these events will be trimmed to nothing and, consequently, not emitted.
    pub whitespace_to_text: bool,

    /// Whether or not CDATA should be converted to text. Default is false.
    ///
    /// If true, instead of `CData` events, `Text` events with the same content will be emitted.
    /// If `trim_whitespace` is also true, these events will be trimmed. If corresponding
    /// CDATA contained nothing but whitespace, this event will be omitted from the stream.
    pub cdata_to_text: bool,

    /// Whether or not comments should be omitted. Default is true.
    ///
    /// If true, `Comment` events will not be emitted at all.
    pub ignore_comments: bool,

    /// Whether or not sequential `Text` should events be merged. Default is true.
    ///
    /// If true, multiple sequential `Text` events will be merged into a single event,
    /// that is, their data will be concatenated.
    ///
    /// Multiple sequential `Text` events are possible if either `cdata_to_text` or `ignore_comments` are set, or
    /// when you have resolvable entities (including built-in entities). Otherwise text events will
    /// always be separated by other events.
    pub coalesce_characters: bool,

    /// A map of extra entities recognized by the reader. Default is an empty map.
    ///
    /// By default the XML parser recognizes the entities defined in the XML spec. Sometimes,
    /// however, it is convenient to make the parser recognize additional entities which
    /// are also not available through the DTD definitions (especially given that at the moment
    /// DTD parsing is not supported).
    pub extra_entities: HashMap<String, String>,

    /// Whether or not the reader should ignore the end of stream. Default is false.
    ///
    /// By default the reader will either error out when it encounters a premature end of
    /// stream, or complete normally if the end of stream was expected. If you want to continue
    /// reading from a stream whose input is supplied progressively, you can set this option to true.
    /// In this case the parser will allow you to pull the next event even if an unexpected end
    /// of stream has happened.
    ///
    /// Note that support for this functionality is incomplete; for example, the parser will fail if
    /// the premature end of stream happens inside PCDATA. Therefore, use this option at your own risk.
    pub ignore_end_of_stream: bool,
}

impl ReaderConfig {
    /// Returns a new config with default values.
    ///
    /// You can tweak default values using builder-like pattern:
    ///
    /// ```rust
    /// use xml::reader::ReaderConfig;
    ///
    /// let config = ReaderConfig::new()
    ///     .trim_whitespace(true)
    ///     .ignore_comments(true)
    ///     .coalesce_characters(false);
    /// ```
    pub fn new() -> ReaderConfig {
        ReaderConfig {
            trim_whitespace: false,
            whitespace_to_text: false,
            cdata_to_text: false,
            ignore_comments: true,
            coalesce_characters: true,
            extra_entities: HashMap::new(),
            ignore_end_of_stream: false,
        }
    }

    /// Adds a new entity mapping and returns an updated config object.
    ///
    /// This is a convenience method for adding external entities mappings to the XML parser.
    /// An example:
    ///
    /// ```rust
    /// use xml::reader::ReaderConfig;
    ///
    /// let mut source: &[u8] = b"...";
    ///
    /// let reader = ReaderConfig::new()
    ///     .add_entity("nbsp", " ")
    ///     .add_entity("copy", "©")
    ///     .add_entity("reg", "®")
    ///     .create_reader(&mut source);
    /// ```
    pub fn add_entity<S: Into<String>, T: Into<String>>(mut self, entity: S, value: T) -> ReaderConfig {
        self.extra_entities.insert(entity.into(), value.into());
        self
    }

    pub fn create_reader_from_str_read<R: StrRead>(self, source: R) -> Reader<R> {
        Reader::new(self, source)
    }

    pub fn create_reader<R: BufRead>(self, source: R) -> Reader<DecodingReader<R>> {
        self.create_reader_from_str_read(DecodingReader::new(source, encoding_rs::UTF_8))
    }
}

impl Default for ReaderConfig {
    fn default() -> ReaderConfig {
        ReaderConfig::new()
    }
}

gen_setters! { ReaderConfig,
    trim_whitespace: val bool,
    whitespace_to_text: val bool,
    cdata_to_text: val bool,
    ignore_comments: val bool,
    coalesce_characters: val bool,
    ignore_end_of_stream: val bool
}
