use std::io::{self, Read};

use encoding_rs::{Decoder, DecoderResult, Encoding};

/// A special kind of buffering wrapper around `Read` which performs decoding of raw bytes according
/// to the specified encoding.
///
/// Rather than providing a raw `read(&[u8])` method, this type provides a `decode_to_str(&mut str)`
/// method, which writes UTF-8 data decoded from the underlying raw stream of bytes.
///
/// This type also handles edge cases of incomplete code points in the underlying stream correctly.
pub struct DecodingReader<R: Read> {
    inner: R,
    decoder: Decoder,
    buf: Box<[u8]>,
    pos: usize,
    cap: usize,
    last_part_decoded: bool,
}

impl<R: Read> DecodingReader<R> {
    /// Wraps the provided `Read` instance and returns a `DecodingReader` which would convert the
    /// provided byte stream to UTF-8 data using the specified encoding.
    ///
    /// The `buf_size` argument specifies the size of the underlying buffer from which decoding
    /// is done. Basically, reading from the underlying `Read` instance happens in chunks of
    /// this size.
    pub fn new(inner: R, encoding: &'static Encoding, buf_size: usize) -> Self {
        assert!(buf_size > 0, "Buffer cannot be empty");
        DecodingReader {
            inner,
            decoder: encoding.new_decoder_with_bom_removal(),
            buf: vec![0; buf_size].into_boxed_slice(),
            pos: 0,
            cap: 0,
            last_part_decoded: false,
        }
    }

    /// Decodes the next chunk of data from the underlying byte stream into UTF-8 data.
    ///
    /// The decoded data is written to the provided mutable string slice. The following values
    /// can be returned from this method:
    ///
    /// * `None`: the underlying stream is empty (EOF).
    /// * `Some(0)`: nothing was written to the provided buffer, but something *was* read from
    ///   the underlying byte stream. This can happen in case the underlying stream contains
    ///   only a part of a single code point. In this situation you must call this method again
    ///   to complete the read process.
    /// * `Some(n)` where `n > 0`: `n` bytes were written to the provided buffer.
    pub fn decode_to_str(&mut self, dst: &mut str) -> io::Result<Option<usize>> {
        // if the interal buffer is empty
        if self.pos == self.cap {
            // fill the internal buffer
            let bytes_read;
            loop {
                match self.inner.read(&mut self.buf) {
                    Ok(n) => {
                        bytes_read = n;
                        break;
                    }
                    Err(ref e) if e.kind() == io::ErrorKind::Interrupted => continue,
                    Err(e) => return Err(e),
                }
            }

            // underlying stream EOF'ed, need to decode pontentially undecoded data in the buffer
            if bytes_read == 0 {
                return self.handle_eof_str(dst);
            }

            // mark the boundaries of the read data
            self.cap = bytes_read;
            self.pos = 0;
        }

        // capture the unprocessed part of the buffer
        let remaining_buf = &self.buf[self.pos..self.cap];

        // decode the buffer to destination
        // last = false, because we're not at input EOF yet, so even if there is a half of a code
        // point in the buffer now, it does not matter - we'll handle it during the next read call
        let (result, bytes_read, bytes_written) =
            self.decoder
                .decode_to_str_without_replacement(remaining_buf, dst, false);
        self.pos += bytes_read;

        match result {
            DecoderResult::InputEmpty | DecoderResult::OutputFull => Ok(Some(bytes_written)),
            DecoderResult::Malformed(_, _) => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Input stream contains byte sequence which is invalid for the configured encoding",
            )),
        }
    }

    fn handle_eof_str(&mut self, dst: &mut str) -> io::Result<Option<usize>> {
        if self.last_part_decoded {
            // We have already decoded the last part in the buffer, return `None` right away.
            Ok(None)
        } else {
            // Complete decoding of a potentially incomplete code point.
            // Note that we explicitly provide an empty slice here, because this method can only
            // be called in case we observe an actual EOF in the input stream, and the internal
            // buffer is empty.
            let (result, bytes_read, bytes_written) = self.decoder.decode_to_str_without_replacement(&[], dst, true);

            match result {
                DecoderResult::InputEmpty => {
                    // Input is consumed completely.
                    self.last_part_decoded = true;
                    Ok(Some(bytes_written))
                }
                DecoderResult::OutputFull => {
                    // The output buffer does not have enough space for the remaining code point.
                    // The client must call this method again with more space in the buffer.
                    Ok(Some(bytes_written))
                }
                DecoderResult::Malformed(_, _) => Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Input stream contains byte sequence which is invalid for the configured encoding",
                )),
            }
        }
    }
}

// TODO: Write tests specifically for DecodingReader.
//       Not urgent since it is mostly covered by the higher-level tests in the parent module.
