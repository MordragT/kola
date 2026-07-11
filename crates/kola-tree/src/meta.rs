use std::marker::PhantomData;

use crate::{
    id::Id,
    node::{AnyId, Column, StorageCheckpoint, UniversalStorage},
};

/// A side-table where every column holds the same type `M`.
/// Derefs to `Storage<M, M, ..., M>` for column access via `Column<T>`.
#[derive(Debug, Clone)]
pub struct UniversalTable<M>(UniversalStorage<M>);

impl<M> UniversalTable<M> {
    pub fn new(cp: StorageCheckpoint) -> Self
    where
        M: Default + Clone,
    {
        Self(UniversalStorage::from_checkpoint(cp))
    }

    pub fn get_any(&self, id: AnyId) -> &M {
        self.0.get_any(id)
    }

    pub fn get<T>(&self, id: Id<T>) -> &M
    where
        UniversalStorage<M>: Column<T, Item = M>,
    {
        self.0.get(id)
    }

    pub fn set<T>(&mut self, id: Id<T>, value: M) -> M
    where
        UniversalStorage<M>: Column<T, Item = M>,
        M: Clone,
    {
        std::mem::replace(self.0.get_mut(id), value)
    }

    pub fn checkpoint(&self) -> UniversalTableCheckpoint {
        UniversalTableCheckpoint(self.0.checkpoint())
    }

    pub fn restore(&mut self, cp: &UniversalTableCheckpoint) {
        self.0.restore(&cp.0);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UniversalTableCheckpoint(StorageCheckpoint);

// ── SecondaryTable ─────────────────────────────────────────────────────────────

/// A simple `Vec<M>` indexed by `Id<T>.as_usize()`, with the node type
/// tracked via `PhantomData<T>`.
#[derive(Debug, Clone)]
pub struct SecondaryTable<T, M> {
    data: Vec<M>,
    _marker: PhantomData<T>,
}

impl<T, M> SecondaryTable<T, M> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn get(&self, id: Id<T>) -> &M {
        &self.data[id.as_usize()]
    }

    pub fn get_mut(&mut self, id: Id<T>) -> &mut M {
        &mut self.data[id.as_usize()]
    }

    pub fn set(&mut self, id: Id<T>, value: M) {
        self.data[id.as_usize()] = value;
    }

    pub fn push(&mut self, value: M) {
        self.data.push(value);
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

impl<T, M> Default for SecondaryTable<T, M> {
    fn default() -> Self {
        Self::new()
    }
}
