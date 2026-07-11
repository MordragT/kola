use std::borrow::Borrow;

use indexmap::IndexMap;
use kola_utils::interner::PathKey;

use crate::{
    id::{Id, SliceId},
    node::{Column, ModuleBody, NodeStorage, StorageCheckpoint},
};

pub type TreeMap = IndexMap<PathKey, Tree>;

#[derive(Debug)]
pub struct TreeBuilder {
    arena: NodeStorage,
}

impl TreeBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn arena(&self) -> &NodeStorage {
        &self.arena
    }

    pub fn arena_mut(&mut self) -> &mut NodeStorage {
        &mut self.arena
    }

    pub fn checkpoint(&self) -> StorageCheckpoint {
        self.arena.checkpoint()
    }

    pub fn restore(&mut self, cp: &StorageCheckpoint) {
        self.arena.restore(cp);
    }

    pub fn alloc<T>(&mut self, val: T) -> Id<T>
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.arena.alloc(val)
    }

    pub fn alloc_slice<T>(&mut self, values: impl IntoIterator<Item = Id<T>>) -> SliceId<T>
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.arena.alloc_slice(values)
    }

    pub fn get<T>(&self, id: Id<T>) -> &T
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.arena.get(id)
    }

    pub fn get_mut<T>(&mut self, id: Id<T>) -> &mut T
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.arena.get_mut(id)
    }

    pub fn get_slice<T>(&self, slice_id: SliceId<T>) -> &[Id<T>]
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.arena.get_slice(slice_id)
    }

    pub fn get_slice_mut<T>(&mut self, slice_id: SliceId<T>) -> &mut [Id<T>]
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.arena.get_slice_mut(slice_id)
    }

    pub fn finish(self, root: Id<ModuleBody>) -> Tree {
        Tree {
            arena: self.arena,
            root,
        }
    }
}

impl Default for TreeBuilder {
    fn default() -> Self {
        Self {
            arena: NodeStorage::default(),
        }
    }
}

impl Borrow<NodeStorage> for TreeBuilder {
    fn borrow(&self) -> &NodeStorage {
        &self.arena
    }
}

#[derive(Debug, Clone)]
pub struct Tree {
    arena: NodeStorage,
    root: Id<ModuleBody>,
}

impl Tree {
    pub fn root_id(&self) -> Id<ModuleBody> {
        self.root
    }

    pub fn arena(&self) -> &NodeStorage {
        &self.arena
    }

    pub fn arena_mut(&mut self) -> &mut NodeStorage {
        &mut self.arena
    }

    pub fn get<T>(&self, id: Id<T>) -> &T
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.arena.get(id)
    }

    pub fn get_slice<T>(&self, slice_id: SliceId<T>) -> &[Id<T>]
    where
        NodeStorage: Column<T, Item = T>,
    {
        self.arena.get_slice(slice_id)
    }
}

impl Borrow<NodeStorage> for Tree {
    fn borrow(&self) -> &NodeStorage {
        &self.arena
    }
}
