use std::borrow::Cow;

use kola_ir::instr::{Func, Symbol};
use kola_protocol::TypeProtocol;
use kola_utils::interner::StrKey;

use crate::{
    arenas::{Arena, RangeArena, StringArena},
    cont::{ContFrame, HeapCont, RawCont},
    env::{HeapEnv, RawEnv},
    handler::{HeapOpClauses, RawOpClauses},
    list::{HeapList, RawList},
    record::{HeapRecord, RawRecord},
    string::{HeapString, RawString},
    value::Value,
    witness::{HeapWitness, RawWitness},
};

#[derive(Debug)]
pub struct Heap {
    strings: StringArena,
    records: RangeArena<(StrKey, Value)>,
    lists: RangeArena<Value>,
    conts: RangeArena<ContFrame>,
    witnesses: Arena<TypeProtocol>,
    environments: RangeArena<(Symbol, Value)>,
    operation_clauses: RangeArena<(StrKey, Func)>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            strings: StringArena::new(),
            records: RangeArena::new(),
            lists: RangeArena::new(),
            conts: RangeArena::new(),
            witnesses: Arena::new(),
            environments: RangeArena::new(),
            operation_clauses: RangeArena::new(),
        }
    }

    #[inline]
    pub fn alloc_string(&mut self, s: &RawString) -> HeapString {
        HeapString(self.strings.alloc(&s.0))
    }

    #[inline]
    pub fn get_string(&self, idx: HeapString) -> RawString<'_> {
        RawString(Cow::Borrowed(self.strings.get(idx.0)))
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
    pub fn alloc_list(&mut self, list: &RawList) -> HeapList {
        HeapList(self.lists.alloc(&list.0))
    }

    #[inline]
    pub fn get_list(&self, list: HeapList) -> RawList<'_> {
        RawList(self.lists.get(list.0))
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
