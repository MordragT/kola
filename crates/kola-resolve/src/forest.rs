use std::{collections::HashMap, rc::Rc};

use kola_tree::{
    id::Id,
    node::Node,
    tree::{Tree, TreeView},
};
use kola_utils::{convert::TryAsRef, interner::PathKey};

#[derive(Debug, Clone, Default)]
pub struct Forest(HashMap<PathKey, Rc<Tree>>);

impl Forest {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn tree(&self, path: PathKey) -> Rc<Tree> {
        self.0[&path].clone()
    }

    pub fn node<T>(&self, path: PathKey, id: Id<T>) -> &T
    where
        Node: TryAsRef<T>,
    {
        let tree = &self.0[&path];
        tree.node(id)
    }

    pub fn iter_nodes(&self, path: PathKey) -> std::slice::Iter<'_, Node> {
        let tree = &self.0[&path];
        tree.iter_nodes()
    }

    pub fn node_count(&self, path: PathKey) -> usize {
        let tree = &self.0[&path];
        tree.count()
    }
}
