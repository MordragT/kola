use std::rc::Rc;

use crate::{
    Phase,
    id::NodeId,
    kind::NodeKind,
    meta::Meta,
    node::{Expr, InnerNode, Node},
};

pub trait NodeContainer {
    fn node<T>(&self, id: NodeId<T>) -> &T
    where
        T: InnerNode;
}

#[derive(Debug)]
pub struct TreeBuilder {
    nodes: Vec<Node>,
}

impl Default for TreeBuilder {
    fn default() -> Self {
        Self { nodes: Vec::new() }
    }
}

impl NodeContainer for TreeBuilder {
    fn node<T>(&self, id: NodeId<T>) -> &T
    where
        T: InnerNode,
    {
        let node = &self.nodes[id.as_usize()];
        T::to_inner_ref(node).unwrap()
    }
}

impl TreeBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn node_mut<T>(&mut self, id: NodeId<T>) -> &mut T
    where
        T: InnerNode,
    {
        let node = &mut self.nodes[id.as_usize()];
        T::to_inner_mut(node).unwrap()
    }

    pub fn update_node<T>(&mut self, id: NodeId<T>, node: T) -> T
    where
        T: InnerNode,
    {
        std::mem::replace(self.node_mut(id), node)
    }

    pub fn insert<T>(&mut self, node: T) -> NodeId<T>
    where
        T: Into<Node>,
    {
        let id = self.nodes.len() as u32;

        let node = node.into();
        self.nodes.push(node);

        NodeId::new(id)
    }

    pub fn finish(self, root: NodeId<Expr>) -> Tree {
        let Self { nodes } = self;

        let nodes = Rc::new(nodes);

        Tree { nodes, root }
    }
}

#[derive(Debug, Clone)]
pub struct Tree {
    nodes: Rc<Vec<Node>>,
    root: NodeId<Expr>,
}

impl NodeContainer for Tree {
    fn node<T>(&self, id: NodeId<T>) -> &T
    where
        T: InnerNode,
    {
        let node = &self.nodes[id.as_usize()];
        T::to_inner_ref(node).unwrap()
    }
}

impl Tree {
    pub fn root_id(&self) -> NodeId<Expr> {
        self.root
    }

    pub fn iter_nodes(&self) -> std::slice::Iter<'_, Node> {
        self.nodes.iter()
    }

    pub fn metadata_with<P>(&self, f: impl Fn(NodeKind) -> Meta<P>) -> Vec<Meta<P>>
    where
        P: Phase,
    {
        self.nodes.iter().map(Node::kind).map(f).collect::<Vec<_>>()
    }
}
