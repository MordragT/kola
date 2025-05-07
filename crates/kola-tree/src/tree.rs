use std::rc::Rc;

use kola_utils::{TryAsMut, TryAsRef};

use crate::{
    id::Id,
    meta::{Meta, Phase},
    node::{Module, Node},
};

pub enum Query2<'a, T, U> {
    V0(Id<T>, &'a T),
    V1(Id<U>, &'a U),
}

pub enum Query3<'a, T, U, V> {
    V0(Id<T>, &'a T),
    V1(Id<U>, &'a U),
    V2(Id<V>, &'a V),
}

pub enum Query4<'a, T, U, V, W> {
    V0(Id<T>, &'a T),
    V1(Id<U>, &'a U),
    V2(Id<V>, &'a V),
    V3(Id<W>, &'a W),
}

pub trait TreeView {
    fn node<T>(&self, id: Id<T>) -> &T
    where
        Node: TryAsRef<T>;

    fn iter_nodes(&self) -> std::slice::Iter<'_, Node>;

    fn query<T>(&self) -> impl Iterator<Item = (Id<T>, &T)>
    where
        T: 'static,
        Node: TryAsRef<T>,
    {
        self.iter_nodes().enumerate().filter_map(|(i, node)| {
            if let Some(t) = node.try_as_ref() {
                Some((Id::from_usize(i), t))
            } else {
                None
            }
        })
    }

    fn query2<T, U>(&self) -> impl Iterator<Item = Query2<'_, T, U>>
    where
        T: 'static,
        U: 'static,
        Node: TryAsRef<T> + TryAsRef<U>,
    {
        self.iter_nodes().enumerate().filter_map(|(i, node)| {
            if let Some(t) = node.try_as_ref() {
                Some(Query2::V0(Id::from_usize(i), t))
            } else if let Some(u) = node.try_as_ref() {
                Some(Query2::V1(Id::from_usize(i), u))
            } else {
                None
            }
        })
    }

    fn query3<T, U, V>(&self) -> impl Iterator<Item = Query3<'_, T, U, V>>
    where
        T: 'static,
        U: 'static,
        V: 'static,
        Node: TryAsRef<T> + TryAsRef<U> + TryAsRef<V>,
    {
        self.iter_nodes().enumerate().filter_map(|(i, node)| {
            if let Some(t) = node.try_as_ref() {
                Some(Query3::V0(Id::from_usize(i), t))
            } else if let Some(u) = node.try_as_ref() {
                Some(Query3::V1(Id::from_usize(i), u))
            } else if let Some(v) = node.try_as_ref() {
                Some(Query3::V2(Id::from_usize(i), v))
            } else {
                None
            }
        })
    }

    fn query4<T, U, V, W>(&self) -> impl Iterator<Item = Query4<'_, T, U, V, W>>
    where
        T: 'static,
        U: 'static,
        V: 'static,
        W: 'static,
        Node: TryAsRef<T> + TryAsRef<U> + TryAsRef<V> + TryAsRef<W>,
    {
        self.iter_nodes().enumerate().filter_map(|(i, node)| {
            if let Some(t) = node.try_as_ref() {
                Some(Query4::V0(Id::from_usize(i), t))
            } else if let Some(u) = node.try_as_ref() {
                Some(Query4::V1(Id::from_usize(i), u))
            } else if let Some(v) = node.try_as_ref() {
                Some(Query4::V2(Id::from_usize(i), v))
            } else if let Some(w) = node.try_as_ref() {
                Some(Query4::V3(Id::from_usize(i), w))
            } else {
                None
            }
        })
    }

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

impl TreeView for TreeBuilder {
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

impl TreeView for Tree {
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
