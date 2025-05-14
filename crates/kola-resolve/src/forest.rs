use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use kola_span::{Loc, Report, SourceManager};
use kola_syntax::loc::{LocPhase, Locations};
use kola_tree::{
    id::Id,
    meta::{MetaCast, MetaView},
    node::{self, Node},
    tree::{Tree, TreeView},
};
use kola_utils::{
    bimap::BiMap, convert::TryAsRef, dependency::DependencyGraph, interner::PathKey, io::FileSystem,
};

use crate::module::{ModuleKey, ModuleScope, UnresolvedModuleScope};

pub struct Forest<Io> {
    pub sources: SourceManager<Io>,
    pub trees: HashMap<PathKey, Rc<Tree>>,
    pub topography: HashMap<PathKey, Rc<Locations>>,
    pub mappings: BiMap<ModuleKey, (PathKey, Id<node::Module>)>,
    pub dependencies: DependencyGraph<ModuleKey>,
    pub unresolved_scopes: HashMap<ModuleKey, UnresolvedModuleScope>,
    pub in_progress: HashSet<ModuleKey>,
    pub scopes: HashMap<ModuleKey, ModuleScope>,
    pub report: Report,
}

impl<Io: FileSystem> Forest<Io> {
    pub fn new(io: Io) -> Self {
        Self {
            sources: SourceManager::new(io),
            trees: HashMap::new(),
            topography: HashMap::new(),
            mappings: BiMap::new(),
            dependencies: DependencyGraph::new(),
            unresolved_scopes: HashMap::new(),
            in_progress: HashSet::new(),
            scopes: HashMap::new(),
            report: Report::new(),
        }
    }

    pub fn tree(&self, path: PathKey) -> Rc<Tree> {
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

    pub fn span<T>(&self, path: PathKey, id: Id<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        let topography = &self.topography[&path];
        *topography.meta(id)
    }
}
