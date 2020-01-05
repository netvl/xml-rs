use std::ops::{Index, Range, RangeFull};

pub struct Buffer {
    data: String,
}

impl Buffer {
    pub fn as_mut(&mut self) -> &mut String {
        &mut self.data
    }

    pub fn reify(&self, slice: BufSlice) -> &str {
        assert!(slice.start <= self.data.len() && slice.end <= self.data.len());
        &self.data[slice.start..slice.end]
    }

    pub fn resolve<'a>(&'a self, slice: &'a str) -> BufSlice {
        assert!(self.check_slice_within_buffer(slice));
        let base_offset = slice.as_ptr() as usize - self.data.as_ptr() as usize;
        BufSlice::new(base_offset, base_offset + slice.len())
    }

    pub fn slice(&self, range: Range<usize>) -> BufSlice {
        assert!(range.start <= self.data.len() && range.end <= self.data.len());
        BufSlice::new(range.start, range.end)
    }

    pub fn slice_from(&self, start: usize) -> BufSlice {
        self.slice(start..self.data.len())
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

impl Index<BufSlice> for Buffer {
    type Output = str;

    fn index(&self, index: BufSlice) -> &str {
        self.reify(index)
    }
}

#[derive(Copy, Clone)]
pub struct BufSlice {
    start: usize,
    end: usize,
}

impl BufSlice {
    fn new(start: usize, end: usize) -> BufSlice {
        BufSlice { start, end }
    }
}

#[derive(Clone)]
pub enum BufCow {
    Slice(BufSlice),
    Owned(String),
}

impl From<BufSlice> for BufCow {
    fn from(slice: BufSlice) -> BufCow {
        BufCow::Slice(slice)
    }
}

impl<T: Into<String>> From<T> for BufCow {
    fn from(value: T) -> BufCow {
        BufCow::Owned(value.into())
    }
}

impl BufCow {
    pub fn into_owned(self, buffer: &Buffer) -> BufCow {
        match self {
            BufCow::Slice(slice) => BufCow::Owned(buffer.reify(slice).to_string()),
            BufCow::Owned(owned) => BufCow::Owned(owned),
        }
    }

    pub fn resolve<'a>(&'a self, buffer: &'a Buffer) -> &'a str {
        match self {
            BufCow::Slice(slice) => &buffer[*slice],
            BufCow::Owned(owned) => owned.as_str(),
        }
    }
}
