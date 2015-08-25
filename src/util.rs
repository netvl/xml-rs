use std::borrow::Borrow;
use std::ops::Deref;

pub trait OptionBorrowExt<T: ?Sized> {
    fn borrow_internals(&self) -> Option<&T>;
}

impl<T: ?Sized, U> OptionBorrowExt<T> for Option<U> where U: Borrow<T> {
    fn borrow_internals(&self) -> Option<&T> {
        self.as_ref().map(Borrow::borrow)
    }
}

pub trait IteratorClonedPairwiseExt: Sized {
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
        let v: Option<String> = Some("abcde".to_string());
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
        v1.insert("a".to_string(), vec![1]);
        v1.insert("b".to_string(), vec![2, 3]);
        v1.insert("c".to_string(), vec![4, 5, 6]);

        let v2: HashMap<String, Vec<usize>> = v1.iter().cloned_pairwise().collect();
        assert_eq!(v1, v2);
    }
}
