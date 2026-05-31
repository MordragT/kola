use std::num::NonZeroU32;

use kola_ir::instr::Symbol;

use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EnvIdx(NonZeroU32);

impl EnvIdx {
    #[inline]
    pub fn as_usize(self) -> usize {
        (self.0.get() - 1) as usize
    }

    #[inline]
    fn make(offset: usize) -> Self {
        Self(NonZeroU32::new((offset + 1) as u32).expect("Arena overflow"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnvArena {
    data: Vec<(Symbol, Value, Option<EnvIdx>)>,
}

impl EnvArena {
    #[inline]
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    #[inline]
    pub fn ambient() -> EnvIdx {
        EnvIdx::make(0) // The ambient environment is at index 0
    }

    /// O(N) Lookup: Walk up the parent chain
    #[inline]
    pub fn get(&self, idx: EnvIdx, name: Symbol) -> Option<Value> {
        let mut idx = Some(idx);
        while let Some(current_idx) = idx {
            let (sym, value, parent) = &self.data[current_idx.as_usize()];
            if *sym == name {
                return Some(*value);
            }
            idx = *parent; // Move up to the outer lexical scope
        }
        None // Variable not found
    }

    pub fn insert(&mut self, parent: EnvIdx, name: Symbol, value: impl Into<Value>) -> EnvIdx {
        let offset = self.data.len();
        self.data.push((name, value.into(), Some(parent)));
        EnvIdx::make(offset)
    }
}
