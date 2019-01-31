use std::borrow::Cow;
use std::fmt;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Name<'a> {
    pub local_name: Cow<'a, str>,
    pub namespace: Option<Cow<'a, str>>,
    pub prefix: Option<Cow<'a, str>>,
}

impl<'a> fmt::Display for Name<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref namespace) = self.namespace {
            write!(f, "{{{}}}", namespace)?;
        }

        if let Some(ref prefix) = self.prefix {
            write!(f, "{}:", prefix)?;
        }

        write!(f, "{}", self.local_name)
    }
}

impl<'a> Name<'a> {
    pub fn local(local_name: impl Into<Cow<'a, str>>) -> Name<'a> {
        Name {
            local_name: local_name.into(),
            namespace: None,
            prefix: None,
        }
    }

    pub fn prefixed(local_name: impl Into<Cow<'a, str>>,
                    prefix: impl Into<Cow<'a, str>>) -> Name<'a> {
        Name {
            local_name: local_name.into(),
            namespace: None,
            prefix: Some(prefix.into()),
        }
    }

    pub fn qualified(local_name: impl Into<Cow<'a, str>>,
                     namespace: impl Into<Cow<'a, str>>,
                     prefix: Option<impl Into<Cow<'a, str>>>) -> Name<'a> {
        Name {
            local_name: local_name.into(),
            namespace: Some(namespace.into()),
            prefix: prefix.map(Into::into),
        }
    }

    pub fn from_str(s: &'a str) -> Option<Name<'a>> {
        let mut it = s.split(':');

        let r = match (it.next(), it.next(), it.next()) {
            (Some(prefix), Some(local_name), None) if !prefix.is_empty() && !local_name.is_empty() =>
                Some((local_name, Some(prefix))),
            (Some(local_name), None, None) if !local_name.is_empty() =>
                Some((local_name, None)),
            (_, _, _) => None
        };
        
        r.map(|(local_name, prefix)| Name {
            local_name: local_name.into(),
            namespace: None,
            prefix: prefix.map(Into::into),
        })
    }

    pub fn to_owned(&self) -> Name<'static> {
        fn clone_cow_str<'a>(cow: &Cow<'a, str>) -> Cow<'static, str> {
            cow.clone().into_owned().into()
        }
        
        Name {
            local_name: clone_cow_str(&self.local_name),
            namespace: self.namespace.as_ref().map(clone_cow_str),
            prefix: self.prefix.as_ref().map(clone_cow_str),
        }
    }
}
