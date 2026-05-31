use std::fmt;

use kola_ir::instr::Tag;
use kola_utils::{display::DisplayWith, interner_ext::InternerExt, serde::SerializeWith};
use serde::ser::SerializeStruct;

use crate::{heap::Heap, value::Value};

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

impl DisplayWith<Heap> for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, heap: &Heap) -> fmt::Result {
        self.tag.fmt(f, &heap.str_interner)?;
        write!(f, "(")?;
        self.value.fmt(f, heap)?;
        write!(f, ")")
    }
}

impl SerializeWith<Heap> for Variant {
    fn serialize<S>(&self, serializer: S, heap: &Heap) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Variant", 2)?;
        state.serialize_field("tag", &self.tag.0)?; // TODO this would need to also consult the StrInterner
        state.serialize_field("value", &heap.with(&*self.value))?;
        state.end()
    }
}
