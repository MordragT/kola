use std::ops::Index;

use derive_more::From;
use kola_collections::HashMap;
use kola_tree::node::{self, ModuleName, ValueName};

use crate::{
    GlobalId,
    symbol::{ModuleSym, ValueSym},
};

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleRef(Vec<ModuleName>);

impl ModuleRef {
    pub fn new(path: Vec<ModuleName>) -> Self {
        Self(path)
    }

    pub fn path(&self) -> &[ModuleName] {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueRef {
    /// The name of the value reference.
    pub name: ValueName,
    /// The global identifier of the path expression that references some other value bind..
    pub global_id: GlobalId<node::PathExpr>,
    /// The symbol of the value bind, this reference occured inside.
    pub source: ValueSym,
}

impl ValueRef {
    pub fn new(name: ValueName, global_id: GlobalId<node::PathExpr>, source: ValueSym) -> Self {
        Self {
            name,
            global_id,
            source,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ModuleRefs {
    modules: HashMap<ModuleSym, ModuleRef>,
    values: Vec<ValueRef>,
}

impl ModuleRefs {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_module(&mut self, sym: ModuleSym, path: Vec<ModuleName>) {
        self.modules.insert(sym, ModuleRef::new(path));
    }

    pub fn insert_value(&mut self, value_ref: ValueRef) {
        self.values.push(value_ref);
    }

    pub fn get_module(&self, sym: ModuleSym) -> Option<&ModuleRef> {
        self.modules.get(&sym)
    }

    pub fn values(&self) -> &[ValueRef] {
        &self.values
    }

    pub fn modules(&self) -> &HashMap<ModuleSym, ModuleRef> {
        &self.modules
    }
}

impl Index<ModuleSym> for ModuleRefs {
    type Output = ModuleRef;

    fn index(&self, index: ModuleSym) -> &Self::Output {
        self.modules
            .get(&index)
            .expect("ModuleSym not found in ModuleRefs")
    }
}
