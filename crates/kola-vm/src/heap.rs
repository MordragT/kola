use std::fmt;

use kola_ir::instr::{Func, Symbol};
use kola_protocol::{TypeInterner, TypeProtocol};
use kola_utils::{
    display::DisplayWith,
    interner::{StrInterner, StrKey},
    serde::SerializeWith,
};
use serde::Serialize;

use crate::{
    arenas::{Arena, RangeArena},
    cont::{ContFrame, HeapCont, RawCont},
    env::{HeapEnv, RawEnv},
    handler::{HeapOpClauses, RawOpClauses},
    list::ListArena,
    record::{HeapRecord, RawRecord},
    string::{StringArena, StringIdx},
    value::Value,
    witness::{HeapWitness, RawWitness},
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
    records: RangeArena<(StrKey, Value)>,
    conts: RangeArena<ContFrame>,
    witnesses: Arena<TypeProtocol>,
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
            records: RangeArena::new(),
            conts: RangeArena::new(),
            witnesses: Arena::new(),
            environments: RangeArena::new(),
            operation_clauses: RangeArena::new(),
        }
    }

    #[inline]
    pub fn alloc_str_key(&mut self, key: StrKey) -> StringIdx {
        let s = self.str_interner[key].as_str();
        self.strings.alloc(s)
    }

    #[inline]
    pub fn alloc_record(&mut self, record: &RawRecord) -> HeapRecord {
        HeapRecord(self.records.alloc(&record.0))
    }

    #[inline]
    pub fn get_record(&self, record: HeapRecord) -> RawRecord<'_> {
        RawRecord(self.records.get(record.0))
    }

    #[inline]
    pub fn alloc_cont(&mut self, cont: &RawCont) -> HeapCont {
        HeapCont(self.conts.alloc(&cont.0))
    }

    #[inline]
    pub fn get_cont(&self, cont: HeapCont) -> RawCont<'_> {
        RawCont(self.conts.get(cont.0))
    }

    #[inline]
    pub fn alloc_witness(&mut self, witness: RawWitness) -> HeapWitness {
        HeapWitness(self.witnesses.alloc(witness.0.into_owned()))
    }

    #[inline]
    pub fn get_witness(&self, witness: HeapWitness) -> RawWitness<'_> {
        RawWitness(self.witnesses.get(witness.0))
    }

    #[inline]
    pub fn alloc_env(&mut self, env: &RawEnv) -> HeapEnv {
        HeapEnv(self.environments.alloc(&env.0))
    }

    #[inline]
    pub fn get_env(&self, env: HeapEnv) -> RawEnv<'_> {
        RawEnv(self.environments.get(env.0))
    }

    #[inline]
    pub fn alloc_op_clauses(&mut self, clauses: &RawOpClauses) -> HeapOpClauses {
        HeapOpClauses(self.operation_clauses.alloc(&clauses.0))
    }

    #[inline]
    pub fn get_op_clauses(&self, clauses: HeapOpClauses) -> RawOpClauses<'_> {
        RawOpClauses(self.operation_clauses.get(clauses.0))
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
