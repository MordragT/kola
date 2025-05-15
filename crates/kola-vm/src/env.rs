use kola_ir::{
    id::InstrId,
    instr::{Atom, Symbol},
};
use std::collections::HashMap;

/// Values in the environment should be fully evaluated (Atom)
#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    pub parent: Option<Box<Env>>,
    pub bindings: HashMap<Symbol, InstrId<Atom>>,
}

impl Env {
    pub fn empty() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: Env) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn lookup(&self, name: &Symbol) -> Option<InstrId<Atom>> {
        if let Some(value) = self.bindings.get(name) {
            Some(*value)
        } else if let Some(parent) = &self.parent {
            parent.lookup(name)
        } else {
            None
        }
    }

    pub fn extend(&mut self, name: Symbol, value: InstrId<Atom>) {
        self.bindings.insert(name, value);
    }
}
