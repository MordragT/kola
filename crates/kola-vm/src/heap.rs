use std::{borrow::Cow, fmt};

use kola_ir::instr::{Func, Symbol, Tag};
use kola_protocol::{TypeInterner, TypeKey, TypeProtocol};
use kola_utils::{
    display::DisplayWith,
    interner::{StrInterner, StrKey},
    serde::SerializeWith,
};
use serde::Serialize;

use crate::{
    arenas::RangeArena,
    env::{HeapEnv, RawEnv},
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
    /// The interner used for symbol to string conversion
    pub str_interner: StrInterner,
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
    environments: RangeArena<(Symbol, Value)>,
    operation_clauses: RangeArena<(StrKey, Func)>,
}

impl Heap {
    pub fn new(str_interner: StrInterner, type_interner: TypeInterner) -> Self {
        Self {
            str_interner,
            type_interner,
            strings: StringArena::new(),
            lists: ListArena::new(),
            records: RecordArena::new(),
            witnesses: WitnessArena::new(),
            variants: VariantArena::new(),
            environments: RangeArena::new(),
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
    pub fn alloc_str_key(&mut self, key: StrKey) -> StringIdx {
        let s = self.str_interner[key].as_str();
        self.strings.alloc(s)
    }

    #[inline]
    pub fn alloc_type_key(&mut self, key: TypeKey) -> WitnessIdx {
        let t = self.type_interner[key].clone();
        self.witnesses.alloc(t)
    }

    #[inline]
    pub fn intern_str<'a>(&mut self, s: impl Into<Cow<'a, str>>) -> StrKey {
        self.str_interner.intern(s)
    }

    #[inline]
    pub fn intern_type<'a>(&mut self, t: impl Into<Cow<'a, TypeProtocol>>) -> TypeKey {
        self.type_interner.intern(t)
    }

    #[inline]
    pub fn get_record_value<'a>(
        &mut self,
        record: RecordIdx,
        field: impl Into<Cow<'a, str>>,
    ) -> Option<Value> {
        let key = self.intern_str(field);
        self.records.get_value(record, key)
    }

    #[inline]
    pub fn record_keys(&mut self, record: RecordIdx) -> ListIdx {
        self.lists.alloc_iter(
            self.records
                .keys(record)
                .map(|k| Value::Str(self.strings.alloc(&self.str_interner[k]))),
        )
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

    // deprecated ------------------

    #[inline]
    pub fn alloc_env(&mut self, env: &RawEnv) -> HeapEnv {
        HeapEnv(self.environments.alloc(&env.0))
    }

    #[inline]
    pub fn get_env(&self, env: HeapEnv) -> RawEnv {
        RawEnv(self.environments.get(env.0).to_vec())
    }

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
