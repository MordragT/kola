use std::{borrow::Cow, ops::Index};

use kola_ir::instr::Symbol;

use crate::{arenas::RangeIdx, heap::Heap, value::Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapEnv(pub RangeIdx<(Symbol, Value)>);

impl HeapEnv {
    pub fn get(self, heap: &Heap) -> RawEnv<'_> {
        heap.get_env(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RawEnv<'a>(pub Cow<'a, [(Symbol, Value)]>);

impl<'a> RawEnv<'a> {
    pub fn new() -> Self {
        Self(Cow::Borrowed(&[]))
    }

    pub fn into_owned(self) -> RawEnv<'static> {
        RawEnv(Cow::Owned(self.0.into_owned()))
    }

    pub fn alloc(&self, heap: &mut Heap) -> HeapEnv {
        heap.alloc_env(self)
    }

    pub fn insert(&mut self, name: Symbol, value: impl Into<Value>) {
        self.0.to_mut().push((name, value.into()));
    }

    pub fn remove(&mut self, name: &Symbol) -> Option<Value> {
        let idx = self.0.iter().position(|(sym, _)| sym == name)?;
        Some(self.0.to_mut().remove(idx).1)
    }

    pub fn get(&self, name: &Symbol) -> Option<&Value> {
        self.0
            .iter()
            .find(|(sym, _)| sym == name)
            .map(|(_, val)| val)
    }
}

impl Index<Symbol> for RawEnv<'_> {
    type Output = Value;

    fn index(&self, index: Symbol) -> &Self::Output {
        self.get(&index).expect("Symbol not found in environment")
    }
}
