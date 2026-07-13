use indexmap::IndexMap;
use kola_utils::interner::PathKey;

use crate::{
    id::Id,
    node::{Column, ModuleBody, NodeStorage, StorageCheckpoint},
    slice::{SliceId, SliceStorage},
};

pub type TreeMap = IndexMap<PathKey, Tree>;

pub trait TreeView {
    fn nodes(&self) -> &NodeStorage;
    fn slices(&self) -> &SliceStorage;

    fn get<T>(&self, id: Id<T>) -> &T
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.nodes().get(id)
    }

    fn get_slice<T>(&self, slice_id: SliceId<T>) -> &[Id<T>]
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.slices().get(slice_id)
    }
}

#[derive(Debug)]
pub struct TreeBuilder {
    pub nodes: NodeStorage,
    pub slices: SliceStorage,
}

impl TreeBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn checkpoint(&self) -> StorageCheckpoint {
        self.nodes.checkpoint()
    }

    pub fn restore(&mut self, cp: &StorageCheckpoint) {
        self.nodes.restore(cp);
    }

    pub fn alloc<T>(&mut self, val: T) -> Id<T>
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.nodes.alloc(val)
    }

    pub fn alloc_slice<T>(&mut self, values: impl IntoIterator<Item = Id<T>>) -> SliceId<T>
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.slices.alloc(values)
    }

    pub fn get<T>(&self, id: Id<T>) -> &T
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.nodes.get(id)
    }

    pub fn get_slice_mut<T>(&mut self, slice_id: SliceId<T>) -> &mut [Id<T>]
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.slices.get_mut(slice_id)
    }

    pub fn finish(self, root: Id<ModuleBody>) -> Tree {
        Tree {
            nodes: self.nodes,
            slices: self.slices,
            root,
        }
    }
}

impl Default for TreeBuilder {
    fn default() -> Self {
        Self {
            nodes: NodeStorage::default(),
            slices: SliceStorage::default(),
        }
    }
}

impl TreeView for TreeBuilder {
    fn nodes(&self) -> &NodeStorage {
        &self.nodes
    }

    fn slices(&self) -> &SliceStorage {
        &self.slices
    }
}

#[derive(Debug, Clone)]
pub struct Tree {
    nodes: NodeStorage,
    slices: SliceStorage,
    root: Id<ModuleBody>,
}

impl Tree {
    pub fn root_id(&self) -> Id<ModuleBody> {
        self.root
    }
}

impl TreeView for Tree {
    fn nodes(&self) -> &NodeStorage {
        &self.nodes
    }

    fn slices(&self) -> &SliceStorage {
        &self.slices
    }
}
