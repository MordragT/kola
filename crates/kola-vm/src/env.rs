use std::ops::Index;

use kola_ir::instr::Symbol;

use crate::{arenas::RangeIdx, heap::Heap, value::Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapEnv(pub RangeIdx<(Symbol, Value)>);

impl HeapEnv {
    pub fn get(self, heap: &Heap) -> RawEnv {
        heap.get_env(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RawEnv(pub Vec<(Symbol, Value)>);

impl RawEnv {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn alloc(&self, heap: &mut Heap) -> HeapEnv {
        heap.alloc_env(self)
    }

    pub fn insert(&mut self, name: Symbol, value: impl Into<Value>) {
        let pos = match self.0.binary_search_by_key(&name, |(sym, _)| *sym) {
            Ok(pos) => pos,
            Err(pos) => pos,
        };

        self.0.insert(pos, (name, value.into()));
    }

    pub fn remove(&mut self, name: Symbol) -> Option<Value> {
        let pos = match self.0.binary_search_by_key(&name, |(sym, _)| *sym) {
            Ok(pos) => pos,
            Err(_) => return None,
        };
        Some(self.0.remove(pos).1)
    }

    pub fn get(&self, name: Symbol) -> Option<Value> {
        let pos = match self.0.binary_search_by_key(&name, |(sym, _)| *sym) {
            Ok(pos) => pos,
            Err(_) => return None,
        };

        Some(self.0[pos].1)
    }
}

impl Index<Symbol> for RawEnv {
    type Output = Value;

    fn index(&self, index: Symbol) -> &Self::Output {
        let pos = self
            .0
            .binary_search_by_key(&index, |(sym, _)| *sym)
            .expect("Symbol not found in environment");
        &self.0[pos].1
    }
}
