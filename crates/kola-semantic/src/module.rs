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
    tree::TreeAccess,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleExplorer<'a, T> {
    tree: &'a T,
    module: &'a ModuleInfo,
    global: &'a HashMap<ModuleId, ModuleInfo>,
}

impl<'a, T> ModuleExplorer<'a, T>
where
    T: TreeAccess,
{
    pub fn new(
        tree: &'a T,
        module: &'a ModuleInfo,
        global: &'a HashMap<ModuleId, ModuleInfo>,
    ) -> Self {
        Self {
            tree,
            module,
            global,
        }
    }

    pub fn explore_path(self, path_id: Id<node::ModulePath>) -> Option<ModuleId> {
        let path = path_id.get(self.tree);
        let mut segments = path.0.iter();

        let first_id = segments.next()?;
        let first = first_id.get(self.tree);
        let mut module_id = self.module.get(&first.0)?.id.clone();

        for id in segments {
            let s = id.get(self.tree);
            module_id = self.global.get(&module_id)?.get(&s.0)?.id.clone();
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
}

pub type ModuleInfo = HashMap<Symbol, ModuleBind>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleBind {
    pub id: ModuleId,
    pub vis: Vis,
    pub kind: ModuleBindKind,
}

impl ModuleBind {
    pub fn import(id: ModuleId, vis: Vis) -> Self {
        Self {
            id,
            vis,
            kind: ModuleBindKind::Import,
        }
    }

    pub fn path(id: ModuleId, vis: Vis) -> Self {
        Self {
            id,
            vis,
            kind: ModuleBindKind::Path,
        }
    }

    pub fn module(id: ModuleId, vis: Vis) -> Self {
        Self {
            id,
            vis,
            kind: ModuleBindKind::Module,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleBindKind {
    Import,
    Path,
    Module,
}
