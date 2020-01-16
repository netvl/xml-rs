use derive_more::From;

use crate::name::Name as ReifiedName;
use crate::reader::model::buffer::{BufSlice, Buffer};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Name {
    pub local_name: BufSlice,
    pub prefix: Option<BufSlice>,
}

impl Name {
    pub fn local(local_name: impl Into<BufSlice>) -> Name {
        Name {
            local_name: local_name.into(),
            prefix: None,
        }
    }

    pub fn maybe_prefixed(local_name: impl Into<BufSlice>, prefix: Option<impl Into<BufSlice>>) -> Name {
        Name {
            local_name: local_name.into(),
            prefix: prefix.map(Into::into),
        }
    }

    pub fn as_reified<'buf>(&self, buffer: &'buf Buffer) -> ReifiedName<'buf> {
        ReifiedName::maybe_prefixed(
            self.local_name.as_reified(buffer),
            self.prefix.map(|p| p.as_reified(buffer)),
        )
    }
}

#[derive(Clone, Debug, From)]
pub enum CowName {
    Ephemeral(Name),
    Reified(ReifiedName<'static>),
}

impl CowName {
    pub fn reify_in_place(&mut self, buffer: &Buffer) {
        match self {
            CowName::Ephemeral(e) => {
                let reified = e.as_reified(buffer).into_owned();
                *self = CowName::Reified(reified);
            }
            CowName::Reified(_) => {} // nothing to do
        }
    }

    pub fn as_reified<'a>(&'a self, buffer: &'a Buffer) -> ReifiedName<'a> {
        match self {
            CowName::Ephemeral(e) => e.as_reified(buffer),
            CowName::Reified(e) => e.as_referenced(),
        }
    }

    pub fn reify(self, buffer: &Buffer) -> ReifiedName {
        match self {
            CowName::Ephemeral(e) => e.as_reified(buffer),
            CowName::Reified(e) => e,
        }
    }
}
