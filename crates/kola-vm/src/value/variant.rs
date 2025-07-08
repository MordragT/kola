use std::fmt;

use kola_ir::instr::Tag;
use kola_utils::{
    interner::StrInterner,
    interner_ext::{DisplayWithInterner, InternerExt, SerializeWithInterner},
};
use serde::ser::SerializeStruct;

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

impl DisplayWithInterner<str> for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        self.tag.fmt(f, interner)?;
        write!(f, "(")?;
        self.value.fmt(f, interner)?;
        write!(f, ")")
    }
}

impl SerializeWithInterner<str> for Variant {
    fn serialize<S>(&self, serializer: S, interner: &StrInterner) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Variant", 2)?;
        state.serialize_field("tag", &self.tag.0)?; // TODO this would need to also consult the StrInterner
        state.serialize_field("value", &interner.with(&*self.value))?;
        state.end()
    }
}
