use std::borrow::Cow;
use std::fmt;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Name<'a> {
    pub local_name: Cow<'a, str>,
    pub prefix: Option<Cow<'a, str>>,
}

impl<'a> fmt::Display for Name<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref prefix) = self.prefix {
            write!(f, "{}:", prefix)?;
        }

        write!(f, "{}", self.local_name)
    }
}

impl<'a> Name<'a> {
    pub fn into_owned(self) -> Name<'static> {
        Name {
            local_name: self.local_name.into_owned().into(),
            prefix: self.prefix.map(Cow::into_owned).map(Into::into),
        }
    }

    pub fn local(local_name: impl Into<Cow<'a, str>>) -> Name<'a> {
        Name {
            local_name: local_name.into(),
            prefix: None,
        }
    }

    pub fn prefixed(local_name: impl Into<Cow<'a, str>>, prefix: impl Into<Cow<'a, str>>) -> Name<'a> {
        Name {
            local_name: local_name.into(),
            prefix: Some(prefix.into()),
        }
    }

    pub fn maybe_prefixed(local_name: impl Into<Cow<'a, str>>, prefix: Option<impl Into<Cow<'a, str>>>) -> Name<'a> {
        Name {
            local_name: local_name.into(),
            prefix: prefix.map(Into::into),
        }
    }

    pub fn from_str(s: &'a str) -> Option<Name<'a>> {
        let mut it = s.split(':');

        let r = match (it.next(), it.next(), it.next()) {
            (Some(prefix), Some(local_name), None) if !prefix.is_empty() && !local_name.is_empty() => {
                Some((local_name, Some(prefix)))
            }
            (Some(local_name), None, None) if !local_name.is_empty() => Some((local_name, None)),
            (_, _, _) => None,
        };

        r.map(|(local_name, prefix)| Name {
            local_name: local_name.into(),
            prefix: prefix.map(Into::into),
        })
    }

    pub fn to_owned(&self) -> Name<'static> {
        fn clone_cow_str<'a>(cow: &Cow<'a, str>) -> Cow<'static, str> {
            cow.clone().into_owned().into()
        }

        Name {
            local_name: clone_cow_str(&self.local_name),
            prefix: self.prefix.as_ref().map(clone_cow_str),
        }
    }

    pub fn as_referenced(&self) -> Name {
        Name {
            local_name: self.local_name.as_ref().into(),
            prefix: self.prefix.as_deref().map(Into::into),
        }
    }
}
