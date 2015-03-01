use std::borrow::Cow;
use std::borrow::Cow::{Borrowed, Owned};
use std::iter::IntoIterator;

use self::Value::{C, S};
use self::Process::{B, O};

enum Value {
    C(char),
    S(&'static str)
}

impl Value {
    fn dispatch(c: char) -> Value {
        match c {
            '<'  => S("&lt;"),
            '>'  => S("&gt;"),
            '"'  => S("&quot;"),
            '\'' => S("&apos;"),
            '&'  => S("&amp;"),
            _    => C(c)
        }
    }
}

enum Process<'a> {
    B(&'a str),
    O(String)
}

impl<'a> Process<'a> {
    fn process(&mut self, (i, next): (usize, Value)) {
        match next {
            S(s) => if let O(ref mut o) = *self {
                o.push_str(s);
            } else if let B(b) = *self {
                let mut r = String::with_capacity(b.len());
                r.push_str(&b[..i]);
                r.push_str(s);
                *self = O(r);
            },
            C(c) => match *self {
                B(_) => {}
                O(ref mut o) => o.push(c)
            }
        }
    }

    fn into_result(self) -> Cow<'a, str> {
        match self {
            B(b) => Borrowed(b),
            O(o) => Owned(o)
        }
    }
}

impl<'a> Extend<Value> for Process<'a> {
    fn extend<I: IntoIterator<Item=Value>>(&mut self, it: I) {
        for v in it.into_iter().enumerate() {
            self.process(v);
        }
    }
}

/// Performs escaping of common XML characters.
///
/// This function replaces several important markup characters with their
/// entity equivalents.
///
/// * `<` → `&lt;`
/// * `>` → `&gt;`
/// * `"` → `&quot;`
/// * `'` → `&apos;`
/// * `&` → `&amp;`
///
/// The resulting string is safe to use inside XML attribute values or in
/// PCDATA sections.
///
/// Does not perform allocations if the given string does not contain escapable characters.
pub fn escape_str(s: &str) -> Cow<str> {
    let mut p = B(s);
    p.extend(s.chars().map(Value::dispatch));
    p.into_result()
}
