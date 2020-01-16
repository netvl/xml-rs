use std::io::{self, BufRead};

use encoding_rs::{Decoder, DecoderResult, Encoding};

/// A special kind of a wrapper around `BufRead` which performs decoding of raw bytes according
/// to the specified encoding.
///
/// Rather than providing a raw `read(&[u8])` method, this type provides a `decode_to_string(&mut String)`
/// method, which writes UTF-8 data decoded from the underlying raw stream of bytes. Amount of data which
/// will be decoded to the mutable string upon each invocation is determined by the size of the `BufRead`'s buffer.
///
/// This type also handles edge cases of incomplete code points in the underlying stream correctly.
pub struct DecodingReader<R: BufRead> {
    inner: R,
    decoder: Decoder,
    last_part_decoded: bool,
}

impl<R: BufRead> DecodingReader<R> {
    /// Wraps the provided `BufRead` instance and returns a `DecodingReader` which would convert the
    /// provided byte stream to UTF-8 data using the specified encoding.
    pub fn new(inner: R, encoding: &'static Encoding) -> Self {
        DecodingReader {
            inner,
            decoder: encoding.new_decoder_with_bom_removal(),
            last_part_decoded: false,
        }
    }

    /// Checks if there is enough space in `dst` to accept more data without reallocation.
    pub fn will_grow(&mut self, dst: &String) -> io::Result<bool> {
        let buffer = self.inner.fill_buf()?;

        let required_dst_space = self
            .decoder
            .max_utf8_buffer_length_without_replacement(buffer.len())
            .unwrap();

        Ok(dst.capacity() - dst.len() < required_dst_space)
    }

    /// Decodes the next chunk of data from the underlying byte stream into UTF-8 data.
    pub fn decode_to_string(&mut self, dst: &mut String) -> io::Result<bool> {
        if self.last_part_decoded {
            return Ok(false);
        }

        let orig_dst_len = dst.len();

        let buffer = self.inner.fill_buf()?;
        let is_eof = buffer.is_empty();

        // Ensure that we never ever get the OutputFull message
        let required_dst_space = self
            .decoder
            .max_utf8_buffer_length_without_replacement(buffer.len())
            .unwrap();
        dst.reserve(required_dst_space);

        let (result, bytes_read) = self.decoder.decode_to_string_without_replacement(buffer, dst, is_eof);
        match result {
            DecoderResult::InputEmpty if is_eof => {
                self.last_part_decoded = true;
                Ok(dst.len() != orig_dst_len)
            }
            DecoderResult::InputEmpty => {
                self.inner.consume(bytes_read);
                Ok(true)
            }
            DecoderResult::OutputFull => panic!("Decoder returned unexpected result variant"),
            DecoderResult::Malformed(_, _) => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Input stream contains byte sequence which is invalid for the configured encoding",
            )),
        }
    }
}

// TODO: Write tests specifically for DecodingReader.
//       Not urgent since it is mostly covered by the higher-level tests in the parent module.
