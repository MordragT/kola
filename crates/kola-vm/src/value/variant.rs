use std::fmt;

use kola_ir::instr::Tag;
use kola_utils::{fmt::DisplayWithInterner, interner::StrInterner};
use serde::{Serialize, ser::SerializeStruct};

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

impl Serialize for Variant {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Variant", 2)?;
        state.serialize_field("tag", &self.tag.0)?; // TODO this would need to also consult the StrInterner
        state.serialize_field("value", &self.value)?;
        state.end()
    }
}
