use std::fmt;
use std::ops::{Deref, DerefMut, Index, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive};

use derive_more::From;

pub struct Buffer {
    data: String,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer { data: String::new() }
    }

    pub fn reify(&self, slice: BufSlice) -> &str {
        if slice.is_static {
            slice.inner
        } else {
            assert!(self.check_slice_within_buffer(slice.inner));
            // Safety note: this transmute (shortening the lifetime from 'static to the buffer) is safe,
            // because `check_slice_within_buffer` ensures that the given slice is within the boundaries of the buffer,
            // Also, because this method takes self by a shared reference, it is guaranteed that there are no
            // outstanding mutable borrows, so aliasing rules are also not violated.
            unsafe { std::mem::transmute(slice.inner) }
        }
    }

    fn check_slice_within_buffer(&self, slice: &str) -> bool {
        let (buf_ptr, buf_len) = (self.data.as_ptr(), self.data.len());
        let buf_end_ptr = buf_ptr.wrapping_offset(buf_len as isize);
        let (slice_ptr, slice_len) = (slice.as_ptr(), slice.len());
        let slice_end_ptr = slice_ptr.wrapping_offset(slice_len as isize);

        let slice_starts_within_buf = buf_ptr <= slice_ptr && slice_ptr <= buf_end_ptr;
        let slice_ends_within_buf = buf_ptr <= slice_end_ptr && slice_end_ptr <= buf_end_ptr;

        slice_starts_within_buf && slice_ends_within_buf
    }
}

impl Deref for Buffer {
    type Target = String;

    fn deref(&self) -> &String {
        &self.data
    }
}

impl DerefMut for Buffer {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.data
    }
}

macro_rules! impl_index {
    ($($t:ty),+) => {
        $(
            impl Index<$t> for Buffer {
                type Output = str;

                fn index(&self, index: $t) -> &Self::Output {
                    &self.data[index]
                }
            }
        )+
    }
}

impl_index!(
    RangeFull,
    Range<usize>,
    RangeFrom<usize>,
    RangeTo<usize>,
    RangeInclusive<usize>,
    RangeToInclusive<usize>
);

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct BufSlice {
    is_static: bool,
    inner: &'static str,
}

impl fmt::Debug for BufSlice {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BufSlice")
            .field("is_static", &self.is_static)
            .field("pointer", &self.inner.as_ptr())
            .field("len", &self.inner.len())
            .finish()
    }
}

impl BufSlice {
    pub fn new(s: &str) -> BufSlice {
        // Safety note: this transmute (extending the lifetime to 'static) is safe,
        // because the only way to use this reference again is via a call to Buffer::reify(), which
        // validates that the slice is contained within the buffer.
        unsafe {
            BufSlice {
                is_static: false,
                inner: std::mem::transmute(s),
            }
        }
    }

    pub fn new_static(s: &'static str) -> BufSlice {
        BufSlice {
            is_static: true,
            inner: s,
        }
    }

    pub fn as_reified(self, buffer: &Buffer) -> &str {
        buffer.reify(self)
    }

    pub fn directly_precedes(&self, other: &BufSlice) -> bool {
        // this slice start + this slice len == other slice start
        self.is_static == other.is_static
            && self.inner.as_ptr().wrapping_offset(self.inner.len() as isize) == other.inner.as_ptr()
    }

    pub fn merge_with_following(&self, other: &BufSlice) -> BufSlice {
        assert!(self.directly_precedes(other));

        BufSlice {
            is_static: self.is_static, // due to the assert above, self.is_static == other.is_static
            // Guaranteed to be safe due to the check above
            inner: unsafe {
                let bytes = std::slice::from_raw_parts(self.inner.as_ptr(), self.inner.len() + other.inner.len());
                std::str::from_utf8_unchecked(bytes)
            },
        }
    }
}

impl<'a> From<&'a str> for BufSlice {
    fn from(s: &'a str) -> Self {
        BufSlice::new(s)
    }
}

#[derive(Debug, Clone, From)]
pub enum BufCow {
    Ephemeral(BufSlice),
    Owned(String),
}

impl BufCow {
    pub fn into_reified(self, buffer: &Buffer) -> String {
        match self {
            BufCow::Ephemeral(slice) => buffer.reify(slice).to_string(),
            BufCow::Owned(owned) => owned,
        }
    }

    pub fn as_reified<'a>(&'a self, buffer: &'a Buffer) -> &'a str {
        match self {
            BufCow::Ephemeral(slice) => slice.as_reified(buffer),
            BufCow::Owned(owned) => owned.as_str(),
        }
    }
}
