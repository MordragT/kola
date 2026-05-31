use std::{array, borrow::Cow, fmt};

use kola_ir::instr::{Func, Tag};
use kola_protocol::{TypeInterner, TypeKey, TypeProtocol};
use kola_utils::{
    display::DisplayWith,
    interner::{StrInterner, StrKey},
    serde::SerializeWith,
};
use serde::Serialize;

use crate::{
    arenas::RangeArena,
    env::EnvArena,
    handler::{HeapOpClauses, RawOpClauses},
    list::{ListArena, ListIdx},
    record::{RecordArena, RecordIdx},
    string::{StringArena, StringIdx},
    value::Value,
    variant::{VariantArena, VariantIdx},
    witness::{WitnessArena, WitnessIdx},
};

#[derive(Debug)]
pub struct Heap {
    /// The type interner used for type reification
    pub type_interner: TypeInterner,
    /// The arena for storing strings
    pub strings: StringArena,
    /// The arena for storing lists
    pub lists: ListArena,
    /// The arena for storing records
    pub records: RecordArena,
    /// The arena for storing witnesses
    pub witnesses: WitnessArena,
    /// The arena for storing variants
    pub variants: VariantArena,
    /// The arena for storing environments
    pub envs: EnvArena,
    operation_clauses: RangeArena<(StrKey, Func)>,
}

impl Heap {
    pub fn new(str_interner: StrInterner, type_interner: TypeInterner) -> Self {
        Self {
            type_interner,
            strings: StringArena::new(str_interner),
            lists: ListArena::new(),
            records: RecordArena::new(),
            witnesses: WitnessArena::new(),
            variants: VariantArena::new(),
            envs: EnvArena::new(),
            operation_clauses: RangeArena::new(),
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

    #[inline]
    pub fn alloc_type_key(&mut self, key: TypeKey) -> WitnessIdx {
        let t = self.type_interner[key].clone();
        self.witnesses.alloc(t)
    }

    #[inline]
    pub fn intern_str<'a>(&mut self, s: impl Into<Cow<'a, str>>) -> StrKey {
        self.strings.interner.intern(s)
    }

    #[inline]
    pub fn intern_type<'a>(&mut self, t: impl Into<Cow<'a, TypeProtocol>>) -> TypeKey {
        self.type_interner.intern(t)
    }

    #[inline]
    pub fn get_record_value<'a>(
        &mut self,
        record: Option<RecordIdx>,
        field: impl Into<Cow<'a, str>>,
    ) -> Option<Value> {
        let key = self.strings.interner.intern(field);
        self.records.get_value(record, key)
    }

    #[inline]
    pub fn record_keys(&mut self, record: Option<RecordIdx>) -> Option<ListIdx> {
        let keys = self
            .records
            .iter(record)
            .map(|(k, _)| Value::Str(StringIdx::Static(k)))
            .collect::<Vec<_>>();
        self.lists.alloc(&keys)
    }

    #[inline]
    pub fn alloc_builtin_variant<'a>(
        &mut self,
        tag: impl Into<Cow<'a, str>>,
        value: Value,
    ) -> VariantIdx {
        let tag = Tag(self.intern_str(tag));
        self.variants.alloc(tag, value)
    }

    // deprecated ----------------

    #[inline]
    pub fn alloc_op_clauses(&mut self, clauses: &RawOpClauses) -> HeapOpClauses {
        HeapOpClauses(self.operation_clauses.alloc(&clauses.0))
    }

    #[inline]
    pub fn get_op_clauses(&self, clauses: HeapOpClauses) -> RawOpClauses<'_> {
        RawOpClauses(Cow::Borrowed(self.operation_clauses.get(clauses.0)))
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
