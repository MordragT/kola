use std::sync::atomic::{AtomicU32, Ordering};

use kola_tree::{id::Id, node};
use kola_utils::{bimap::BiMap, interner::StrKey};

static GENERATOR: AtomicU32 = AtomicU32::new(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleKey(u32);

impl ModuleKey {
    pub fn new() -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
        Self(id)
    }
}

#[derive(Debug, Clone)]
pub struct ModuleScope {
    pub module_key: ModuleKey,
    pub modules: BiMap<StrKey, Id<node::ModuleBind>>,
    pub values: BiMap<StrKey, Id<node::ValueBind>>,
    pub types: BiMap<StrKey, Id<node::TypeBind>>,
}

impl ModuleScope {
    pub fn new(module_key: ModuleKey) -> Self {
        Self {
            module_key,
            modules: BiMap::new(),
            values: BiMap::new(),
            types: BiMap::new(),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ScopeStack {
    pub reverse: Vec<ModuleScope>,
    pub scopes: Vec<ModuleScope>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            reverse: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn push(&mut self, scope: ModuleScope) {
        self.scopes.push(scope);
    }

    pub fn pop(&mut self) -> Option<ModuleScope> {
        if let Some(scope) = self.scopes.pop() {
            // Store the scope in reverse order for later use
            self.reverse.push(scope.clone());
            Some(scope)
        } else {
            None
        }
    }

    pub fn current(&self) -> Option<&ModuleScope> {
        self.scopes.last()
    }

    pub fn current_mut(&mut self) -> &mut ModuleScope {
        self.scopes.last_mut().unwrap()
    }
}
