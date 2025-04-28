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
pub enum PathResolution {
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
    pub fn path_expr(self, path_id: Id<node::PathExpr>) -> PathResolution {
        let path = path_id.get(self.tree);
        let mut segments = path.0.iter();

        let Some(first_id) = segments.next() else {
            return PathResolution::Unresolved;
        };
        let first = first_id.get(self.tree);
        let Some(bind) = self.module.module(&first.0) else {
            return PathResolution::Unresolved;
        };
        let mut module_id = bind.id.clone();

        while let Some(id) = segments.next() {
            let s = id.get(self.tree);

            if let Some(module) = self
                .global
                .get(&module_id)
                .and_then(|info| info.module(&s.0))
                && module.vis == Vis::Export
            {
                module_id = module.id.clone();
            } else {
                let mut remaining = segments.copied().collect::<Vec<_>>();
                remaining.insert(0, *id);

                return PathResolution::Partial {
                    module_id,
                    remaining,
                };
            }
        }

        PathResolution::Resolved(module_id)
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
        let mut module_id = self.module.module(&first.0)?.id.clone();

        // TODO should I allow access to private modules of a parent in a DIRECT child ?
        // The way I have implemented this currently it would not work.
        // I guess for helper modules this would be pretty usefull
        for id in segments {
            let s = id.get(self.tree);
            let module = self.global.get(&module_id)?.module(&s.0)?;

            if module.vis != Vis::Export {
                return None;
            }
            module_id = module.id.clone();
        }

        Some(module_id)
    }
}

pub type ModuleInfoTable = HashMap<ModuleId, ModuleInfo>;

pub trait ModuleInfoView {
    fn module(&self, symbol: &Symbol) -> Option<&ModuleBind>;
    fn value(&self, symbol: &Symbol) -> Option<&ValueBind>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleInfoBuilder {
    pub modules: HashMap<Symbol, ModuleBind>,
    pub values: HashMap<Symbol, ValueBind>,
}

impl ModuleInfoBuilder {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            values: HashMap::new(),
        }
    }

    // TODO needs to check for special symbols
    // E.g. "super" module defined here should not be overriden
    // Maybe do not allow to override at all and return error then here
    pub fn insert_module(&mut self, symbol: Symbol, bind: ModuleBind) -> Option<ModuleBind> {
        self.modules.insert(symbol, bind)
    }

    // TODO should also probably fail on override
    pub fn insert_value(&mut self, symbol: Symbol, bind: ValueBind) -> Option<ValueBind> {
        self.values.insert(symbol, bind)
    }

    pub fn finish(self) -> ModuleInfo {
        let Self { modules, values } = self;

        ModuleInfo {
            modules: Rc::new(modules),
            values: Rc::new(values),
        }
    }
}

impl ModuleInfoView for ModuleInfoBuilder {
    fn module(&self, symbol: &Symbol) -> Option<&ModuleBind> {
        self.modules.get(symbol)
    }

    fn value(&self, symbol: &Symbol) -> Option<&ValueBind> {
        self.values.get(symbol)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleInfo {
    pub modules: Rc<HashMap<Symbol, ModuleBind>>,
    pub values: Rc<HashMap<Symbol, ValueBind>>,
}

impl ModuleInfoView for ModuleInfo {
    fn module(&self, symbol: &Symbol) -> Option<&ModuleBind> {
        self.modules.get(symbol)
    }

    fn value(&self, symbol: &Symbol) -> Option<&ValueBind> {
        self.values.get(symbol)
    }
}

// TODO I could include type information for values and modules
// if they are speficically written out. Not sure if there is a huge benefit though
// May be a bit more performant than to do determine it later in the Typer
// But is not strictly the main goal here so might as well not do it.

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueBind {
    pub id: Id<node::Expr>,
    pub vis: Vis,
}

impl ValueBind {
    pub fn new(id: Id<node::Expr>, vis: Vis) -> Self {
        Self { id, vis }
    }
}

// // TODO visibilty
// pub struct TypeBind {
//     pub id: Id<node::Type>,
// }

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
