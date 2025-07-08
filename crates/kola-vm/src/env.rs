use std::ops::Index;

use kola_collections::ImHashMap;
use kola_ir::instr::Symbol;

use crate::value::Value;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Env(ImHashMap<Symbol, Value>);

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, name: Symbol, value: impl Into<Value>) {
        self.0.insert(name, value.into());
    }

    pub fn remove(&mut self, name: &Symbol) -> Option<Value> {
        self.0.remove(name)
    }

    pub fn get(&self, name: &Symbol) -> Option<&Value> {
        self.0.get(name)
    }
}

impl Index<Symbol> for Env {
    type Output = Value;

    fn index(&self, index: Symbol) -> &Self::Output {
        self.0.get(&index).expect("Symbol not found in environment")
    }
}
