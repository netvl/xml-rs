use std::ops::{Deref, DerefMut};

pub enum StrBuffer<'a> {
    Borrowed(&'a mut str),
    Owned(Box<str>),
}

impl StrBuffer<'static> {
    pub fn new_owned(size: usize) -> Self {
        StrBuffer::Owned(String::from_utf8(vec![0; size]).unwrap().into_boxed_str())
    }
}

impl<'a> StrBuffer<'a> {
    pub fn new_borrowed(inner: &'a mut str) -> Self {
        StrBuffer::Borrowed(inner)
    }
}

impl<'a> Deref for StrBuffer<'a> {
    type Target = str;

    fn deref(&self) -> &str {
        match self {
            StrBuffer::Borrowed(slice) => slice,
            StrBuffer::Owned(slice) => slice,
        }
    }
}

impl<'a> DerefMut for StrBuffer<'a> {
    fn deref_mut(&mut self) -> &mut str {
        match self {
            StrBuffer::Borrowed(slice) => slice,
            StrBuffer::Owned(slice) => slice,
        }
    }
}

pub enum Buffer<'a> {
    Borrowed(&'a mut [u8]),
    Owned(Box<[u8]>),
}

impl Buffer<'static> {
    pub fn new_owned(size: usize) -> Self {
        Buffer::Owned(vec![0; size].into_boxed_slice())
    }
}

impl<'a> Buffer<'a> {
    pub fn new_borrowed(inner: &'a mut [u8]) -> Self {
        Buffer::Borrowed(inner)
    }
}

impl<'a> Deref for Buffer<'a> {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        match self {
            Buffer::Borrowed(slice) => slice,
            Buffer::Owned(slice) => slice,
        }
    }
}

impl<'a> DerefMut for Buffer<'a> {
    fn deref_mut(&mut self) -> &mut [u8] {
        match self {
            Buffer::Borrowed(slice) => slice,
            Buffer::Owned(slice) => slice,
        }
    }
}

