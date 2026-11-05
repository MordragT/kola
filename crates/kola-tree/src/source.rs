use std::fmt;

use indexmap::IndexMap;
use kola_span::{Loc, SourceId};

use crate::tree::Tree;

/// Parser/source-level identity for a module AST.
///
/// This is deliberately distinct from the resolver's `ModuleSym`, which
/// identifies a semantic module instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct ModuleKey(u32);

impl ModuleKey {
    #[inline]
    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    #[inline]
    pub const fn as_usize(self) -> usize {
        self.0 as usize
    }

    #[inline]
    pub const fn id(self) -> u32 {
        self.0
    }
}

impl fmt::Display for ModuleKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "module{}", self.0)
    }
}

impl From<u32> for ModuleKey {
    #[inline]
    fn from(id: u32) -> Self {
        Self::new(id)
    }
}

/// The set of module ASTs belonging to one source file.
///
/// A source file has one root module and may contain any number of inline
/// modules. Each module is a self-contained `Tree`, so AST IDs never cross a
/// module boundary. Only `ModuleKey` crosses.
#[derive(Debug, Clone)]
pub struct SourceTrees {
    pub root: ModuleKey,
    pub trees: IndexMap<ModuleKey, Tree<Loc>>,
}

impl SourceTrees {
    #[inline]
    pub fn new(root: ModuleKey) -> Self {
        Self {
            root,
            trees: IndexMap::new(),
        }
    }

    #[inline]
    pub fn from_root(root: ModuleKey, tree: Tree<Loc>) -> Self {
        let mut trees = IndexMap::new();
        trees.insert(root, tree);
        Self { root, trees }
    }

    #[inline]
    pub fn insert(&mut self, key: ModuleKey, tree: Tree<Loc>) -> Option<Tree<Loc>> {
        self.trees.insert(key, tree)
    }

    #[inline]
    pub fn get(&self, key: ModuleKey) -> Option<&Tree<Loc>> {
        self.trees.get(&key)
    }

    #[inline]
    pub fn get_mut(&mut self, key: ModuleKey) -> Option<&mut Tree<Loc>> {
        self.trees.get_mut(&key)
    }

    #[inline]
    pub fn root_tree(&self) -> &Tree<Loc> {
        self.get(self.root).expect("root module tree is missing")
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.trees.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.trees.is_empty()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (ModuleKey, &Tree<Loc>)> + '_ {
        self.trees.iter().map(|(&key, tree)| (key, tree))
    }
}

/// All parsed source files.
pub type TreeMap = IndexMap<SourceId, SourceTrees>;
