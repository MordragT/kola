use std::fmt;

use kola_utils::{display::DisplayWith, interner::StrInterner, serde::SerializeWith};
use serde::Serialize;

use crate::{
    env::EnvArena, list::ListArena, record::RecordArena, string::StringArena, variant::VariantArena,
};

/// Configurable capacities for your runtime arenas to eliminate execution-time reallocations.
#[derive(Debug, Clone, Copy)]
pub struct HeapCapacities {
    pub envs: usize,
    pub records: usize,
    pub lists: usize,
    pub variants: usize,
    /// Backing store size in bytes for dynamic runtime string data (e.g., 64 KB)
    pub strings_bytes: usize,
    /// Number of structural string pieces/nodes to pre-allocate
    pub strings_nodes: usize,
}

impl Default for HeapCapacities {
    fn default() -> Self {
        Self {
            envs: 4096,               // Enough for deep lexical scoping chains and loops
            records: 4096,            // High structural sharing means nodes stack up fast
            lists: 2048,              // Tail-allocated lists or list spines
            variants: 1024,           // Typically lower volume than records/environments
            strings_bytes: 64 * 1024, // 65,536 bytes of dynamic character space
            strings_nodes: 1024,      // 1,024 rope/slice tracking descriptors
        }
    }
}

#[derive(Debug)]
pub struct Heap {
    /// The arena for storing strings
    pub strings: StringArena,
    /// The arena for storing lists
    pub lists: ListArena,
    /// The arena for storing records
    pub records: RecordArena,
    /// The arena for storing variants
    pub variants: VariantArena,
    /// The arena for storing environments
    pub envs: EnvArena,
}

impl Heap {
    /// Creates a fresh runtime heap with optimized default capacities.
    pub fn new(str_interner: StrInterner) -> Self {
        Self::with_capacities(HeapCapacities::default(), str_interner)
    }

    /// Allows passing custom capacities (e.g., tiny limits for testing, massive limits for compiler flags).
    pub fn with_capacities(caps: HeapCapacities, str_interner: StrInterner) -> Self {
        Self {
            strings: StringArena::with_capacity(
                caps.strings_bytes,
                caps.strings_nodes,
                str_interner,
            ),
            lists: ListArena::with_capacity(caps.lists),
            records: RecordArena::with_capacity(caps.records),
            variants: VariantArena::with_capacity(caps.variants),
            envs: EnvArena::with_capacity(caps.envs),
        }
    }

    #[inline]
    pub fn with<'a, T>(&'a self, value: &'a T) -> WithHeap<'a, T> {
        WithHeap::new(value, self)
    }

    #[inline]
    pub fn to_json<T>(&self, value: &T) -> Result<String, serde_json::Error>
    where
        T: SerializeWith<Self>,
    {
        serde_json::to_string_pretty(&self.with(value))
    }
}

pub struct WithHeap<'a, T> {
    pub value: &'a T,
    pub heap: &'a Heap,
}

impl<'a, T> WithHeap<'a, T> {
    pub fn new(value: &'a T, heap: &'a Heap) -> Self {
        Self { value, heap }
    }
}

impl<T> fmt::Display for WithHeap<'_, T>
where
    T: DisplayWith<Heap>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f, self.heap)
    }
}

impl<T> Serialize for WithHeap<'_, T>
where
    T: SerializeWith<Heap>,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer, self.heap)
    }
}
