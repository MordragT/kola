use indexmap::IndexMap;
use kola_interner::PathKey;

use crate::{
    id::Id,
    node::{ModuleBody, NodeStorage, StorageCheckpoint},
    query::{Col, Get},
    slice::{SliceId, SliceStorage},
};

pub type TreeMap = IndexMap<PathKey, Tree>;

#[derive(Debug, Clone, Copy)]
pub struct TreeCheckpoint {
    pub nodes: StorageCheckpoint,
    pub slices: usize,
}

#[derive(Debug)]
pub struct Tree {
    pub nodes: NodeStorage,
    pub slices: SliceStorage,
}

impl Tree {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn checkpoint(&self) -> TreeCheckpoint {
        TreeCheckpoint {
            nodes: self.nodes.checkpoint(),
            slices: self.slices.checkpoint(),
        }
    }

    pub fn restore(&mut self, cp: &TreeCheckpoint) {
        self.nodes.restore(&cp.nodes);
        self.slices.restore(cp.slices);
    }

    pub fn alloc<T>(&mut self, val: T) -> Id<T>
    where
        NodeStorage: Col<T, Column = Vec<T>>,
    {
        self.nodes.alloc(val)
    }

    pub fn alloc_slice<T>(&mut self, values: impl IntoIterator<Item = Id<T>>) -> SliceId<T>
    where
        NodeStorage: Get<T, Item = T>,
    {
        self.slices.alloc(values)
    }

    pub fn get<T>(&self, id: Id<T>) -> &T
    where
        NodeStorage: Get<T, Item = T>,
    {
        self.nodes.get(id)
    }

    pub fn get_slice_mut<T>(&mut self, slice_id: SliceId<T>) -> &mut [Id<T>]
    where
        NodeStorage: Get<T, Item = T>,
    {
        self.slices.get_mut(slice_id)
    }

    pub fn root_id(&self) -> Id<ModuleBody> {
        <NodeStorage as Col<ModuleBody>>::ids(&self.nodes)
            .next()
            .expect("Tree has no root node")
    }
}

impl Default for Tree {
    fn default() -> Self {
        Self {
            nodes: NodeStorage::default(),
            slices: SliceStorage::default(),
        }
    }
}
