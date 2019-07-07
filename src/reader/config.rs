use std::io::BufRead;
use crate::reader::str_read::StrRead;
use crate::reader::parsing::Reader;
use crate::reader::decoding_reader::DecodingReader;

pub struct ReaderConfig {
    pub trim_whitespace: bool,

    pub whitespace_to_characters: bool,

    pub cdata_to_characters: bool,

    pub ignore_comments: bool,

    pub coalesce_characters: bool,

    pub ignore_end_of_stream: bool,
}

impl ReaderConfig {
    pub fn new() -> ReaderConfig {
        ReaderConfig {
            trim_whitespace: false,
            whitespace_to_characters: false,
            cdata_to_characters: false,
            ignore_comments: true,
            coalesce_characters: true,
            ignore_end_of_stream: false,
        }
    }

    pub fn reader_from_str_read<R: StrRead>(self, source: R) -> Reader<R> {
        Reader::new(self, source)
    }

    pub fn reader_from_buf_read<R: BufRead>(self, source: R) -> Reader<DecodingReader<R>> {
        self.reader_from_str_read(DecodingReader::new(source, encoding_rs::UTF_8))
    }
}

impl Default for ReaderConfig {
    fn default() -> ReaderConfig {
        ReaderConfig::new()
    }
}

gen_setters! { ReaderConfig,
    trim_whitespace: val bool,
    whitespace_to_characters: val bool,
    cdata_to_characters: val bool,
    ignore_comments: val bool,
    coalesce_characters: val bool,
    ignore_end_of_stream: val bool
}
