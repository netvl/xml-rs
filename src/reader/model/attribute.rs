use crate::attribute::Attribute as ReifiedAttribute;
use crate::reader::model::buffer::{BufSlice, Buffer};
use crate::reader::model::name::Name;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Attribute {
    pub name: Name,
    pub value: BufSlice,
}

impl Attribute {
    pub fn new(name: Name, value: impl Into<BufSlice>) -> Attribute {
        Attribute {
            name,
            value: value.into(),
        }
    }

    pub fn as_reified<'buf>(&self, buffer: &'buf Buffer) -> ReifiedAttribute<'buf> {
        ReifiedAttribute::new(self.name.as_reified(buffer), self.value.as_reified(buffer))
    }
}
