use std::{ops::Index, rc::Rc};

use kola_collections::ImHashMap;
use kola_ir::instr::Symbol;
use kola_utils::interner::{StrInterner, StrKey};

use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    pub bindings: ImHashMap<Symbol, Value>,
    pub interner: Rc<StrInterner>,
}

impl Env {
    pub fn new(interner: impl Into<Rc<StrInterner>>) -> Self {
        Self {
            bindings: ImHashMap::new(),
            interner: interner.into(),
        }
    }

    pub fn insert(&mut self, name: Symbol, value: Value) {
        self.bindings.insert(name, value);
    }

    pub fn get(&self, name: &Symbol) -> Option<&Value> {
        self.bindings.get(name)
    }

    pub fn get_str(&self, key: StrKey) -> Option<&str> {
        self.interner.get(key)
    }

    pub fn interner(&self) -> Rc<StrInterner> {
        self.interner.clone()
    }
}

impl Index<Symbol> for Env {
    type Output = Value;

    fn index(&self, index: Symbol) -> &Self::Output {
        self.bindings
            .get(&index)
            .expect("Symbol not found in environment")
    }
}

impl Index<StrKey> for Env {
    type Output = str;

    fn index(&self, index: StrKey) -> &Self::Output {
        self.interner
            .get(index)
            .expect("String key not found in interner")
    }
}
