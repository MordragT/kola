use kola_tree::{
    id::Id,
    node,
    tree::TreeView,
    visit::{Visitable, Visitor},
};
use kola_utils::{
    bimap::BiMap,
    interner::{PathKey, StrKey},
    io::FileSystem,
};
use std::ops::{ControlFlow, Deref};

use crate::{forest::Forest, module::ModuleKey};

#[derive(Debug, Clone)]
pub struct Scope {
    pub module_key: ModuleKey,
    pub modules: BiMap<StrKey, Id<node::ModuleBind>>,
    pub values: BiMap<StrKey, Id<node::ValueBind>>,
    pub types: BiMap<StrKey, Id<node::TypeBind>>,
}

impl Scope {
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
    pub reverse: Vec<Scope>,
    pub scopes: Vec<Scope>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            reverse: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn push(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    pub fn pop(&mut self) -> Option<Scope> {
        if let Some(scope) = self.scopes.pop() {
            // Store the scope in reverse order for later use
            self.reverse.push(scope.clone());
            Some(scope)
        } else {
            None
        }
    }

    pub fn current(&self) -> Option<&Scope> {
        self.scopes.last()
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct Declare {
    pub path_key: PathKey,
    pub scopes: ScopeStack,
}

impl Declare {
    pub fn new(path_key: PathKey) -> Self {
        Self {
            path_key,
            scopes: ScopeStack::new(),
        }
    }

    pub fn declare<Io>(&mut self, forest: &mut Forest<Io>)
    where
        Io: FileSystem,
    {
        let tree = forest.tree(self.path_key);

        // Create a visitor to walk the tree and collect declarations
        let mut declarer = Declarer {
            forest,
            state: self,
        };

        match tree.root_id().visit_by(&mut declarer, tree.deref()) {
            ControlFlow::Continue(()) => (),
            ControlFlow::Break(_) => unreachable!(),
        }
    }
}

pub struct Declarer<'a, Io> {
    pub forest: &'a mut Forest<Io>,
    pub state: &'a mut Declare,
}

impl<'a, T, Io> Visitor<T> for Declarer<'a, Io>
where
    T: TreeView,
    Io: FileSystem,
{
    type BreakValue = !;

    fn visit_module(&mut self, id: Id<node::Module>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let module_key = ModuleKey::new();

        // Store the new module in our modules mapping
        self.forest
            .mappings
            .insert(module_key, (self.state.path_key, id));

        // Add dependency from current module to this new module
        if let Some(current_scope) = self.state.scopes.current() {
            self.forest
                .dependencies
                .add_dependency(current_scope.module_key, module_key);
        }

        // Create a new scope for this module
        self.state.scopes.push(Scope::new(module_key));

        // Walk through children nodes
        self.walk_module(id, tree)?;

        // Pop the scope now that we're done with this module
        self.state.scopes.pop();

        ControlFlow::Continue(())
    }

    fn visit_module_import(
        &mut self,
        id: Id<node::ModuleImport>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let bind = tree.node(id);
        let name = tree.node(bind.name);

        // Register the module binding in the current scope
        self.state.scopes.current_mut().modules.insert(name.0, id);

        self.walk_module_bind(id, tree)
    }

    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let bind = tree.node(id);
        let name = tree.node(bind.name);

        // Register the value binding in the current scope
        self.state.scopes.current_mut().values.insert(name.0, id);

        self.walk_value_bind(id, tree)
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let bind = tree.node(id);
        let name = tree.node(bind.name);

        // Register the type binding in the current scope
        self.state.scopes.current_mut().types.insert(name.0, id);

        self.walk_type_bind(id, tree)
    }
}
