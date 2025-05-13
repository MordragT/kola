use std::{collections::HashMap, rc::Rc};

use kola_span::SourceManager;
use kola_syntax::loc::Locations;
use kola_tree::{
    id::Id,
    node::{self, Node},
    tree::{Tree, TreeView},
};
use kola_utils::{
    bimap::BiMap, convert::TryAsRef, dependency::DependencyGraph, interner::PathKey, io::FileSystem,
};

use crate::module::ModuleKey;

// pub type Ptr<T> = Arc<T>;
pub type Ptr<T> = Rc<T>;

pub struct Forest<Io> {
    pub sources: SourceManager<Io>,
    pub trees: HashMap<PathKey, Ptr<Tree>>,
    pub topography: HashMap<PathKey, Ptr<Locations>>,
    pub mappings: BiMap<ModuleKey, (PathKey, Id<node::Module>)>,
    pub dependencies: DependencyGraph<ModuleKey>,
}

impl<Io: FileSystem> Forest<Io> {
    pub fn new(io: Io) -> Self {
        Self {
            sources: SourceManager::new(io),
            trees: HashMap::new(),
            topography: HashMap::new(),
            mappings: BiMap::new(),
            dependencies: DependencyGraph::new(),
        }
    }

    pub fn tree(&self, path: PathKey) -> Ptr<Tree> {
        self.trees[&path].clone()
    }

    pub fn node<T>(&self, path: PathKey, id: Id<T>) -> &T
    where
        Node: TryAsRef<T>,
    {
        let tree = &self.trees[&path];
        tree.node(id)
    }

    pub fn iter_nodes(&self, path: PathKey) -> std::slice::Iter<'_, Node> {
        let tree = &self.trees[&path];
        tree.iter_nodes()
    }

    pub fn node_count(&self, path: PathKey) -> usize {
        let tree = &self.trees[&path];
        tree.count()
    }
}
