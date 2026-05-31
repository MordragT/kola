use std::{fmt, num::NonZeroU32};

use kola_ir::instr::Tag;
use kola_utils::{display::DisplayWith, serde::SerializeWith};
use serde::ser::SerializeStruct;

use crate::{heap::Heap, value::Value};

/// A `Copy`-friendly index into a `VariantArena`.
///
/// Stores the tag directly (so tag access is free without heap lookup)
/// and a `NonZeroU32` index into the arena's backing `Vec<(Tag, Value)>`
/// to retrieve the inner value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariantIdx {
    tag: Tag,
    idx: NonZeroU32,
}

impl VariantIdx {
    /// The variant's tag — accessible without the heap.
    #[inline]
    pub fn tag(self) -> Tag {
        self.tag
    }

    #[inline]
    fn idx(self) -> usize {
        self.idx.get() as usize - 1
    }

    #[inline]
    fn make(idx: usize, tag: Tag) -> Self {
        Self {
            tag,
            idx: NonZeroU32::new((idx + 1) as u32).expect("variant arena overflow"),
        }
    }
}

impl DisplayWith<Heap> for VariantIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, heap: &Heap) -> fmt::Result {
        self.tag.fmt(f, &heap.strings.interner)?;
        write!(f, "(")?;
        heap.variants.get_value(*self).fmt(f, heap)?;
        write!(f, ")")
    }
}

impl SerializeWith<Heap> for VariantIdx {
    fn serialize<S>(&self, serializer: S, heap: &Heap) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Variant", 2)?;
        state.serialize_field("tag", &heap.strings.interner[self.tag.0])?;
        state.serialize_field("value", &heap.with(&heap.variants.get_value(*self)))?;
        state.end()
    }
}

/// An append-only arena for variants.
///
/// Stores `(Tag, Value)` pairs. Variants are never modified after allocation.
#[derive(Debug, Clone)]
pub struct VariantArena {
    data: Vec<(Tag, Value)>,
}

impl VariantArena {
    #[inline]
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    /// Allocate a new variant with the given tag and value.
    ///
    /// Returns a `VariantIdx` that is `Copy`.
    #[inline]
    pub fn alloc(&mut self, tag: Tag, value: Value) -> VariantIdx {
        let idx = self.data.len();
        self.data.push((tag, value));
        VariantIdx::make(idx, tag)
    }

    /// Get the inner value of a variant.
    #[inline]
    pub fn get_value(&self, idx: VariantIdx) -> Value {
        self.data[idx.idx()].1
    }

    /// Get the tag of a variant from the arena.
    /// Prefer `idx.tag()` which is cheaper.
    #[inline]
    pub fn get_tag(&self, idx: VariantIdx) -> Tag {
        self.data[idx.idx()].0
    }
}

impl Default for VariantArena {
    fn default() -> Self {
        Self::new()
    }
}
