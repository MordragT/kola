// use indexmap::IndexMap;
// use kola_tree::node::Symbol;

// use crate::env::{KindEnv, TypeEnv};

// /*

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
// pub type ModuleEnv = IndexMap<Symbol, Module>;

// pub struct Module {
//     types: TypeEnv,
//     kinds: KindEnv,
//     modules: ModuleEnv,
//     module_types: ModuleTypeEnv,
// }

// pub type ModuleTypeEnv = IndexMap<Symbol, ModuleType>;

// pub struct ModuleType {
//     types: TypeEnv,
//     kinds: KindEnv,
//     module_types: ModuleTypeEnv,
// }

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use kola_tree::{
    id::Id,
    node::{self, Symbol, Vis},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId {
    pub parent: Option<Box<Self>>,
    pub path: PathBuf,
    pub id: Id<node::Module>,
}

impl ModuleId {
    pub fn root(path: impl Into<PathBuf>, id: Id<node::Module>) -> Self {
        Self {
            parent: None,
            path: path.into(),
            id,
        }
    }

    pub fn new(parent: Self, path: impl Into<PathBuf>, id: Id<node::Module>) -> Self {
        Self {
            parent: Some(Box::new(parent)),
            path: path.into(),
            id,
        }
    }

    // TODO better name maybe import dir ?
    // The working directory for the module e.g.
    // a/b for a/b.kl
    pub fn work_dir(&self) -> PathBuf {
        let stem = self.path.file_stem().unwrap();

        self.path.parent().unwrap().join(stem)
    }
}

/// For file-module: ModuleId's path is current path parent. For inline: ModuleId's path is current path
/// For file-module: ModuleId's path is current path + child symbol. For inline: Cannot import files in inline modules.
/// For file-module & inline-module: ModuleId's path is current path
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct ModuleInfo {
//     pub parent: Option<ModuleId>,
//     pub bindings: HashMap<Symbol, ModuleBind>,
// }

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
