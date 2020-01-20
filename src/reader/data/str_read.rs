use std::io::{self, BufRead};

/// Describes types which can read UTF-8 data to the provided buffer.
///
/// This type is similar to `std::io::Read`, but is designed to work with UTF-8-compliant data, and the
/// target buffer is intentionally growable.
pub trait StrRead {
    /// Checks whether the target buffer will be reallocated if `read_str_data` is called on it now.
    ///
    /// Returns `true` if the next read into the provided buffer will result in its capacity increase and
    /// therefore reallocation of the backing storage, `false` otherwise.
    fn will_increase_capacity(&mut self, dst: &String) -> io::Result<bool>;

    /// Reads a UTF-8 piece of data into the provided buffer.
    ///
    /// Note that the actual amount of data being read is not defined, but it is guaranteed that it will be bounded.
    /// In the most common case, it would be equal to the underlying buffer capacity, which is usually 4-8 KiB.
    ///
    /// Returns a boolean value indicating whether there is more data to read. In other words, if
    /// this method returns `false`, then the reader has reached the end of the stream.
    fn read_str_data(&mut self, dst: &mut String) -> io::Result<bool>;
}

// Below implementation is not valid, it does not handle data being spread across the chunk boundary

impl<R: BufRead> StrRead for R {
    fn will_increase_capacity(&mut self, dst: &String) -> io::Result<bool> {
        todo!()
        //        let buffer = self.fill_buf()?;
        //
        //        let valid_str_len = valid_utf8_prefix(buffer)?.len();
        //
        //        Ok(dst.capacity() - dst.len() < valid_str_len)
    }

    fn read_str_data(&mut self, dst: &mut String) -> io::Result<bool> {
        todo!()
        //        let buffer = self.fill_buf()?;
        //        if buffer.is_empty() {
        //            return Ok(false);
        //        }
        //
        //        let valid_str = valid_utf8_prefix(buffer)?;
        //        dst.push_str(valid_str); // can reallocate
        //        self.consume(valid_str.len());
        //
        //        Ok(true)
    }
}

//fn valid_utf8_prefix(data: &[u8]) -> io::Result<&str> {
//    match std::str::from_utf8(data) {
//        Ok(s) => Ok(s),
//        Err(e) => {
//            let valid_boundary = e.valid_up_to();
//            match e.error_len() {
//                Some(n) => {
//                    return Err(io::Error::new(
//                        io::ErrorKind::InvalidData,
//                        format!(
//                            "Input buffer contains an illegal UTF-8 sequence at index {}: {:?}",
//                            valid_boundary,
//                            &buffer[valid_boundary..valid_boundary + n]
//                        ),
//                    ))
//                }
//                // Guaranteed to be safe because of the `valid_up_to()` method contract
//                None => unsafe { std::str::from_utf8_unchecked(&buffer[..valid_boundary]) },
//            }
//        }
//    }
//}
