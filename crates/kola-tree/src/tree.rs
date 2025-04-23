use std::rc::Rc;

use kola_utils::{TryAsMut, TryAsRef};

use crate::{
    id::Id,
    meta::{Meta, Phase},
    node::{Module, Node},
};

pub trait TreeAccess {
    fn node<T>(&self, id: Id<T>) -> &T
    where
        Node: TryAsRef<T>;

    fn iter_nodes(&self) -> std::slice::Iter<'_, Node>;

    fn metadata_with<P>(&self, f: impl Fn(&Node) -> Meta<P>) -> Vec<Meta<P>>
    where
        P: Phase,
    {
        self.iter_nodes().map(f).collect::<Vec<_>>()
    }
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

impl TreeAccess for TreeBuilder {
    fn node<T>(&self, id: Id<T>) -> &T
    where
        Node: TryAsRef<T>,
    {
        let node = &self.nodes[id.as_usize()];
        node.try_as_ref().unwrap()
    }

    fn iter_nodes(&self) -> std::slice::Iter<'_, Node> {
        self.nodes.iter()
    }
}

impl TreeBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn node_mut<T>(&mut self, id: Id<T>) -> &mut T
    where
        Node: TryAsMut<T>,
    {
        let node = &mut self.nodes[id.as_usize()];
        node.try_as_mut().unwrap()
    }

    pub fn update_node<T>(&mut self, id: Id<T>, node: T) -> T
    where
        Node: TryAsMut<T>,
    {
        std::mem::replace(self.node_mut(id), node)
    }

    pub fn insert<T>(&mut self, node: T) -> Id<T>
    where
        Node: From<T>,
    {
        let id = self.nodes.len() as u32;

        let node = node.into();
        self.nodes.push(node);

        Id::new(id)
    }

    pub fn finish(self, root: Id<Module>) -> Tree {
        let Self { nodes } = self;

        let nodes = Rc::new(nodes);

        Tree { nodes, root }
    }
}

#[derive(Debug, Clone)]
pub struct Tree {
    nodes: Rc<Vec<Node>>,
    root: Id<Module>,
}

impl TreeAccess for Tree {
    fn node<T>(&self, id: Id<T>) -> &T
    where
        Node: TryAsRef<T>,
    {
        let node = &self.nodes[id.as_usize()];
        node.try_as_ref().unwrap()
    }

    fn iter_nodes(&self) -> std::slice::Iter<'_, Node> {
        self.nodes.iter()
    }
}

impl Tree {
    pub fn root_id(&self) -> Id<Module> {
        self.root
    }
}
