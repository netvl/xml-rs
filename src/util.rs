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

pub trait IteratorClonedPairwiseExt<'a, K, V> {
    fn cloned_pairwise(self) -> ClonedPairwise<'a, Self, K, V>;
}

impl<'a, I, K, V> IteratorClonedPairwiseExt<'a, K, V> for I
        where I: Iterator<(&'a K, &'a V)>,
              K: Clone, V: Clone {
    fn cloned_pairwise(self) -> ClonedPairwise<'a, I, K, V> {
        ClonedPairwise(self)
    }
}

pub struct ClonedPairwise<'a, I: Iterator<(&'a K, &'a V)>, K: Clone, V: Clone>(I);

impl<'a, I, K, V> Iterator<(K, V)> for ClonedPairwise<'a, I, K, V>
    where I: Iterator<(&'a K, &'a V)>,
          K: Clone + 'a,
          V: Clone + 'a {
    fn next(&mut self) -> Option<(K, V)> {
        self.0.next().map(|(k, v)| (k.clone(), v.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::{OptionBorrowExt, IntoOwned, IteratorClonedPairwiseExt};

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

    #[test]
    fn test_cloned_pairwise() {
        use std::collections::HashMap;

        let mut v1: HashMap<String, Vec<uint>> = HashMap::new();
        v1.insert("a".into_string(), vec![1]);
        v1.insert("b".into_string(), vec![2, 3]);
        v1.insert("c".into_string(), vec![4, 5, 6]);

        let v2: HashMap<String, Vec<uint>> = v1.iter().cloned_pairwise().collect();
        assert_eq!(v1, v2);
    }
}
