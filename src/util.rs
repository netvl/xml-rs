use std::borrow::{BorrowFrom, ToOwned};

pub trait OptionBorrowExt<Sized? T, U> where T: BorrowFrom<U> {
    fn borrow_internals(&self) -> Option<&T>;
}

impl<Sized? T, U> OptionBorrowExt<T, U> for Option<U> where T: BorrowFrom<U> {
    fn borrow_internals(&self) -> Option<&T> {
        self.as_ref().map(BorrowFrom::borrow_from)
    }
}

pub trait IntoOwned<O> {
    fn into_owned(self) -> O;
}

impl<'a, O, Sized? T, S> IntoOwned<O> for S where S: IntoCow<'a, O, T>, T: ToOwned<O> + 'a {
    #[inline]
    fn into_owned(self) -> O {
        self.into_cow().into_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::{OptionBorrowExt, IntoOwned};

    #[test]
    fn test_borrow_value() {
        let v: Option<int> = Some(10);
        let r: Option<&int> = v.borrow_internals();
        assert!(r.is_some());
        assert_eq!(*r.unwrap(), 10);

        let v: Option<int> = None;
        let r: Option<&int> = v.borrow_internals();
        assert!(r.is_none());
    }

    #[test]
    fn test_borrow_string() {
        let v: Option<String> = Some("abcde".into_string());
        let r: Option<&str> = v.borrow_internals();
        assert!(r.is_some());
        assert_eq!(r.unwrap(), "abcde");

        let v: Option<String> = None;
        let r: Option<&str> = v.borrow_internals();
        assert!(r.is_none());
    }

    #[test]
    fn test_into_owned() {
        let v1: String = "abcde".into_owned();
        let v2: String = "abcde".into_string().into_owned();
        assert_eq!(v1, v2);
    }
}
