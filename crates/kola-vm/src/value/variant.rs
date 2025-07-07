use std::fmt;

use kola_ir::instr::Tag;
use kola_utils::{fmt::DisplayWithInterner, interner::StrInterner};

use super::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    tag: Tag,
    value: Box<Value>,
}

impl Variant {
    #[inline]
    pub fn new(tag: Tag, value: Value) -> Self {
        Self {
            tag,
            value: Box::new(value),
        }
    }

    #[inline]
    pub fn tag(&self) -> Tag {
        self.tag
    }

    #[inline]
    pub fn value(&self) -> &Value {
        &self.value
    }
}

impl DisplayWithInterner for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        self.tag.fmt(f, interner)?;
        write!(f, "(")?;
        self.value.fmt(f, interner)?;
        write!(f, ")")
    }
}
