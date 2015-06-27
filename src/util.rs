use std::borrow::Borrow;
use std::ops::Deref;
use std::io::{self, Read};
use std::str;
use std::fmt;

pub trait OptionBorrowExt<T: ?Sized> {
    fn borrow_internals(&self) -> Option<&T>;
}

impl<T: ?Sized, U> OptionBorrowExt<T> for Option<U> where U: Borrow<T> {
    fn borrow_internals(&self) -> Option<&T> {
        self.as_ref().map(Borrow::borrow)
    }
}

pub trait IteratorClonedPairwiseExt {
    fn cloned_pairwise(self) -> ClonedPairwise<Self>;
}

impl<I, RK, RV, K, V> IteratorClonedPairwiseExt for I
        where I: Iterator<Item=(RK, RV)>,
              RK: Deref<Target=K>, RV: Deref<Target=V>,
              K: Clone, V: Clone {
    fn cloned_pairwise(self) -> ClonedPairwise<I> {
        ClonedPairwise(self)
    }
}

pub struct ClonedPairwise<I>(I);

impl<I, RK, RV, K, V> Iterator for ClonedPairwise<I>
    where I: Iterator<Item=(RK, RV)>,
          RK: Deref<Target=K>,
          RV: Deref<Target=V>,
          K: Clone,
          V: Clone, {

    type Item = (K, V);

    fn next(&mut self) -> Option<(K, V)> {
        self.0.next().map(|(k, v)| (k.clone(), v.clone()))
    }
}

#[derive(Debug)]
pub enum CharReadError {
    UnexpectedEof,
    Utf8(str::Utf8Error),
    Io(io::Error)
}

impl From<str::Utf8Error> for CharReadError {
    fn from(e: str::Utf8Error) -> CharReadError {
        CharReadError::Utf8(e)
    }
}

impl From<io::Error> for CharReadError {
    fn from(e: io::Error) -> CharReadError {
        CharReadError::Io(e)
    }
}

impl fmt::Display for CharReadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CharReadError::*;
        match *self {
            UnexpectedEof => write!(f, "unexpected end of stream"),
            Utf8(ref e) => write!(f, "UTF-8 decoding error: {}", e),
            Io(ref e) => write!(f, "I/O error: {}", e)
        }
    }
}

pub fn next_char_from<R: Read>(source: &mut R) -> Result<Option<char>, CharReadError> {
    const MAX_CODEPOINT_LEN: usize = 4;
    
    let mut buf = [0u8; MAX_CODEPOINT_LEN];

    let mut pos = 0;
    loop {
        let next = match source.bytes().next() {
            Some(Ok(b)) => b,
            Some(Err(e)) => return Err(e.into()),
            None if pos == 0 => return Ok(None),
            None => return Err(CharReadError::UnexpectedEof)
        };
        buf[pos] = next;
        pos += 1;

        match str::from_utf8(&buf[..pos]) {
            Ok(s) => return Ok(s.chars().next()),  // always Some(..)
            Err(_) if pos < MAX_CODEPOINT_LEN => {},
            Err(e) => return Err(e.into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{OptionBorrowExt, IteratorClonedPairwiseExt};

    #[test]
    fn test_borrow_value() {
        let v: Option<isize> = Some(10);
        let r: Option<&isize> = v.borrow_internals();
        assert!(r.is_some());
        assert_eq!(*r.unwrap(), 10);

        let v: Option<isize> = None;
        let r: Option<&isize> = v.borrow_internals();
        assert!(r.is_none());
    }

    #[test]
    fn test_borrow_string() {
        let v: Option<String> = Some("abcde".into());
        let r: Option<&str> = v.borrow_internals();
        assert!(r.is_some());
        assert_eq!(r.unwrap(), "abcde");

        let v: Option<String> = None;
        let r: Option<&str> = v.borrow_internals();
        assert!(r.is_none());
    }

    #[test]
    fn test_cloned_pairwise() {
        use std::collections::HashMap;

        let mut v1: HashMap<String, Vec<usize>> = HashMap::new();
        v1.insert("a".into(), vec![1]);
        v1.insert("b".into(), vec![2, 3]);
        v1.insert("c".into(), vec![4, 5, 6]);

        let v2: HashMap<String, Vec<usize>> = v1.iter().cloned_pairwise().collect();
        assert_eq!(v1, v2);
    }

    #[test]
    fn test_next_char_from() {
        use std::io;
        use std::error::Error;

        let mut bytes: &[u8] = "correct".as_bytes();    // correct ASCII
        assert_eq!(super::next_char_from(&mut bytes).unwrap(), Some('c'));

        let mut bytes: &[u8] = "правильно".as_bytes();  // correct BMP
        assert_eq!(super::next_char_from(&mut bytes).unwrap(), Some('п'));

        let mut bytes: &[u8] = "😊".as_bytes();          // correct non-BMP
        assert_eq!(super::next_char_from(&mut bytes).unwrap(), Some('😊'));

        let mut bytes: &[u8] = b"";                     // empty 
        assert_eq!(super::next_char_from(&mut bytes).unwrap(), None);

        let mut bytes: &[u8] = b"\xf0\x9f\x98";         // incomplete code point
        match super::next_char_from(&mut bytes).unwrap_err() {
            super::CharReadError::UnexpectedEof => {},
            e => panic!("Unexpected result: {:?}", e)
        };

        let mut bytes: &[u8] = b"\xff\x9f\x98\x32";     // invalid code point
        match super::next_char_from(&mut bytes).unwrap_err() {
            super::CharReadError::Utf8(_) => {},
            e => panic!("Unexpected result: {:?}", e)
        };


        // error during read
        struct ErrorReader;
        impl io::Read for ErrorReader {
            fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
                Err(io::Error::new(io::ErrorKind::Other, "test error"))
            }
        }

        let mut r = ErrorReader;
        match super::next_char_from(&mut r).unwrap_err() {
            super::CharReadError::Io(ref e) if e.kind() == io::ErrorKind::Other && 
                                               e.description() == "test error" => {},
            e => panic!("Unexpected result: {:?}", e)
        }
    }
}
