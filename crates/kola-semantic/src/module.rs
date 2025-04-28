// *   For functors like `functor (S : SIG) => Body`, `S` acts precisely like a **Module Variable**.
//     It's a name that represents an unknown module *argument* that is required to conform to the signature `SIG`.
// *   When you *apply* this functor to a concrete module, say `MyFunctor(MyModule)`,
//     conceptually, you are indeed "substituting" `S` with `MyModule` inside the `Body`.
//     The most common and elegant way to implement this is not through literal text or AST substitution,
//     but by **extending the environment**.
//     When you process the `Body` of the functor application `MyFunctor(MyModule)`,
//     you perform this processing in an environment where the name `S` is bound to the module `MyModule`.
// *   This environment binding means that any path like `S.x` or `S.T` encountered within the `Body`
//     is resolved by first looking up `S` in the environment (finding `MyModule`)
//     and then looking up `x` or `T` within `MyModule`.
// */
use std::{
    collections::HashMap,
    fmt,
    path::{Path, PathBuf},
    rc::Rc,
};

use kola_tree::{
    id::Id,
    node::{self, Symbol, Vis},
    tree::TreeView,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModulePathResolution {
    Unresolved,
    Partial {
        module_id: ModuleId,
        remaining: Vec<Id<node::Name>>,
    },
    Resolved(ModuleId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleExplorer<'a, T, M> {
    pub tree: &'a T,
    pub module_id: &'a ModuleId,
    pub module: &'a M,
    pub global: &'a HashMap<ModuleId, ModuleInfo>,
}

impl<'a, T, M> ModuleExplorer<'a, T, M>
where
    T: TreeView,
    M: ModuleInfoView,
{
    pub fn new(
        tree: &'a T,
        module_id: &'a ModuleId,
        module: &'a M,
        global: &'a HashMap<ModuleId, ModuleInfo>,
    ) -> Self {
        Self {
            tree,
            module_id,
            module,
            global,
        }
    }

    /// Resolves as much as possible from the path expression
    ///
    /// * Returns `Unresolved` if the first segment was not a submodule defined in the current module.
    /// * Returns `Partial` if some segments could be resolved but there are remaining segments.
    /// * Returns `Resolved` if all segments could be resolved.
    pub fn path_expr(self, path_id: Id<node::PathExpr>) -> ModulePathResolution {
        let path = path_id.get(self.tree);
        let mut segments = path.0.iter();

        let Some(first_id) = segments.next() else {
            return ModulePathResolution::Unresolved;
        };
        let first = first_id.get(self.tree);
        let Some(bind) = self.module.lookup(&first.0) else {
            return ModulePathResolution::Unresolved;
        };
        let mut module_id = bind.id.clone();

        while let Some(id) = segments.next() {
            let s = id.get(self.tree);

            if let Some(module) = self
                .global
                .get(&module_id)
                .and_then(|info| info.lookup(&s.0))
                && module.vis == Vis::Export
            {
                module_id = module.id.clone();
            } else {
                let mut remaining = segments.copied().collect::<Vec<_>>();
                remaining.insert(0, *id);

                return ModulePathResolution::Partial {
                    module_id,
                    remaining,
                };
            }
        }

        ModulePathResolution::Resolved(module_id)
    }

    /// Resolves the module path
    /// 1. Looks for module of the first segment in the current module
    /// 2. For every other segment looks in the corresponding module
    ///     and checks if a module exists for it and if it is marked as `export`
    ///
    /// Returns None if some segment of the path could not be resolved
    pub fn module_path(self, path_id: Id<node::ModulePath>) -> Option<ModuleId> {
        let path = path_id.get(self.tree);
        let mut segments = path.0.iter();

        let first_id = segments.next()?;
        let first = first_id.get(self.tree);
        let mut module_id = self.module.lookup(&first.0)?.id.clone();

        for id in segments {
            let s = id.get(self.tree);
            let module = self.global.get(&module_id)?.lookup(&s.0)?;

            if module.vis != Vis::Export {
                return None;
            }
            module_id = module.id.clone();
        }

        Some(module_id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Repr {
    parent: Option<ModuleId>,
    path: PathBuf,
    id: Id<node::Module>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(Rc<Repr>);

impl fmt::Display for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let parent = if let Some(parent) = &self.0.parent {
            Some(&parent.0.path)
        } else {
            None
        };

        f.debug_struct("ModuleId")
            .field("id", &self.0.id.as_usize())
            .field("path", &self.0.path)
            .field("parent", &parent)
            .finish()
    }
}

impl ModuleId {
    pub fn root(path: impl Into<PathBuf>, id: Id<node::Module>) -> Self {
        let repr = Repr {
            parent: None,
            path: path.into(),
            id,
        };

        Self(Rc::new(repr))
    }

    pub fn new(parent: Self, path: impl Into<PathBuf>, id: Id<node::Module>) -> Self {
        let repr = Repr {
            parent: Some(parent),
            path: path.into(),
            id,
        };

        Self(Rc::new(repr))
    }

    pub fn parent(&self) -> Option<&Self> {
        self.0.parent.as_ref()
    }

    pub fn path(&self) -> &Path {
        &self.0.path
    }

    pub fn id(&self) -> Id<node::Module> {
        self.0.id
    }

    pub fn is_parent_of(&self, other: &Self) -> bool {
        other.parent().is_some_and(|parent| parent == self)
    }

    pub fn is_child_of(&self, other: &Self) -> bool {
        self.parent().is_some_and(|parent| parent == other)
    }
}

pub type ModuleInfoTable = HashMap<ModuleId, ModuleInfo>;

pub trait ModuleInfoView {
    fn lookup(&self, symbol: &Symbol) -> Option<&ModuleBind>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleInfoBuilder {
    pub binds: HashMap<Symbol, ModuleBind>,
}

impl ModuleInfoBuilder {
    pub fn new() -> Self {
        Self {
            binds: HashMap::new(),
        }
    }

    pub fn insert(&mut self, symbol: Symbol, bind: ModuleBind) -> Option<ModuleBind> {
        self.binds.insert(symbol, bind)
    }

    pub fn finish(self) -> ModuleInfo {
        ModuleInfo::new(self.binds)
    }
}

impl ModuleInfoView for ModuleInfoBuilder {
    fn lookup(&self, symbol: &Symbol) -> Option<&ModuleBind> {
        self.binds.get(symbol)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleInfo {
    pub binds: Rc<HashMap<Symbol, ModuleBind>>,
}

impl ModuleInfo {
    pub fn new(binds: HashMap<Symbol, ModuleBind>) -> Self {
        Self {
            binds: Rc::new(binds),
        }
    }
}

impl ModuleInfoView for ModuleInfo {
    fn lookup(&self, symbol: &Symbol) -> Option<&ModuleBind> {
        self.binds.get(symbol)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleBind {
    pub id: ModuleId,
    pub vis: Vis,
}

impl ModuleBind {
    pub fn new(id: ModuleId, vis: Vis) -> Self {
        Self { id, vis }
    }
}
