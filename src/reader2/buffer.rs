use std::ops::{Deref, DerefMut};

pub struct Buffer {
    pub(super) buf: String,
    pub(super) len: usize,
}

impl Deref for Buffer {
    type Target = str;

    fn deref(&self) -> &str {
        &self.buf[..self.len]
    }
}

impl DerefMut for Buffer {
    fn deref_mut(&mut self) -> &mut str {
        &mut self.buf[..self.len]
    }
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            buf: String::new(),
            len: 0,
        }
    }

    pub fn clear(&mut self) {
        self.len = 0;
        self.buf.clear();
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn at_idx(&self, idx: usize) -> char {
        self[idx..].chars().next().unwrap()
    }

    pub fn first(&self) -> char {
        self[..].chars().next().unwrap()
    }

    pub fn last(&self) -> char {
        self[..].chars().rev().next().unwrap()
    }

    pub fn last_str(&self) -> &str {
        let (idx, _) = self[..].char_indices().rev().next().unwrap();
        &self[idx..]
    }
}
