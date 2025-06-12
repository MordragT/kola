use std::{collections::HashMap, ops::Index, rc::Rc};

use kola_span::SourceId;
use kola_tree::{
    id::Id,
    node::Node,
    tree::{Tree, TreeView},
};
use kola_utils::convert::TryAsRef;

#[derive(Debug, Clone, Default)]
pub struct Forest(HashMap<SourceId, Rc<Tree>>);

impl Forest {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn tree(&self, source: SourceId) -> Rc<Tree> {
        self.0[&source].clone()
    }

    pub fn insert(&mut self, source: SourceId, tree: Tree) {
        self.0.insert(source, Rc::new(tree));
    }

    pub fn node<T>(&self, source: SourceId, id: Id<T>) -> &T
    where
        Node: TryAsRef<T>,
    {
        let tree = &self.0[&source];
        tree.node(id)
    }

    pub fn iter_nodes(&self, source: SourceId) -> std::slice::Iter<'_, Node> {
        let tree = &self.0[&source];
        tree.iter_nodes()
    }

    pub fn node_count(&self, source: SourceId) -> usize {
        let tree = &self.0[&source];
        tree.count()
    }
}

impl Index<SourceId> for Forest {
    type Output = Rc<Tree>;

    fn index(&self, source: SourceId) -> &Self::Output {
        self.0.get(&source).unwrap()
    }
}
