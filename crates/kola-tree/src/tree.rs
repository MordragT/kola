use std::rc::Rc;

use kola_utils::{TryAsMut, TryAsRef};

use crate::{
    id::NodeId,
    meta::{Meta, Phase},
    node::{Module, Node},
};

pub trait NodeContainer {
    fn node<T>(&self, id: NodeId<T>) -> &T
    where
        Node: TryAsRef<T>;
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
        Node: TryAsRef<T>,
    {
        let node = &self.nodes[id.as_usize()];
        node.try_as_ref().unwrap()
    }
}

impl TreeBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn node_mut<T>(&mut self, id: NodeId<T>) -> &mut T
    where
        Node: TryAsMut<T>,
    {
        let node = &mut self.nodes[id.as_usize()];
        node.try_as_mut().unwrap()
    }

    pub fn update_node<T>(&mut self, id: NodeId<T>, node: T) -> T
    where
        Node: TryAsMut<T>,
    {
        std::mem::replace(self.node_mut(id), node)
    }

    pub fn insert<T>(&mut self, node: T) -> NodeId<T>
    where
        Node: From<T>,
    {
        let id = self.nodes.len() as u32;

        let node = node.into();
        self.nodes.push(node);

        NodeId::new(id)
    }

    pub fn finish(self, root: NodeId<Module>) -> Tree {
        let Self { nodes } = self;

        let nodes = Rc::new(nodes);

        Tree { nodes, root }
    }
}

#[derive(Debug, Clone)]
pub struct Tree {
    nodes: Rc<Vec<Node>>,
    root: NodeId<Module>,
}

impl NodeContainer for Tree {
    fn node<T>(&self, id: NodeId<T>) -> &T
    where
        Node: TryAsRef<T>,
    {
        let node = &self.nodes[id.as_usize()];
        node.try_as_ref().unwrap()
    }
}

impl Tree {
    pub fn root_id(&self) -> NodeId<Module> {
        self.root
    }

    pub fn iter_nodes(&self) -> std::slice::Iter<'_, Node> {
        self.nodes.iter()
    }

    pub fn metadata_with<P>(&self, f: impl Fn(&Node) -> Meta<P>) -> Vec<Meta<P>>
    where
        P: Phase,
    {
        self.nodes.iter().map(f).collect::<Vec<_>>()
    }

    // pub(crate) fn get<T>(&self, id: NodeId<T>) -> &Node {
    //     &self.nodes[id.as_usize()]
    // }
}
