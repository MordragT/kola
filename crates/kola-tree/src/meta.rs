use std::{
    collections::{HashMap, hash_map},
    iter::Copied,
    marker::PhantomData,
    ops::{Index, IndexMut},
    slice, vec,
};

use crate::{
    col::{Col, Get, GetOpt},
    id::{Id, IdIter},
    node::{AnyId, StorageCheckpoint, UniversalStorage},
};

/// A side-table where every column holds the same type `M`.
#[derive(Debug, Clone)]
pub struct MetaSet<M>(UniversalStorage<M>);

impl<M> MetaSet<M> {
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
        UniversalStorage<M>: Get<T, Item = M>,
    {
        self.0.get(id)
    }

    pub fn set<T>(&mut self, id: Id<T>, value: M) -> M
    where
        UniversalStorage<M>: Get<T, Item = M>,
        M: Clone,
    {
        std::mem::replace(self.0.get_mut(id), value)
    }

    pub fn checkpoint(&self) -> MetaSetCheckpoint {
        MetaSetCheckpoint(self.0.checkpoint())
    }

    pub fn restore(&mut self, cp: &MetaSetCheckpoint) {
        self.0.restore(&cp.0);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MetaSetCheckpoint(StorageCheckpoint);

/// A simple `Vec<M>` indexed by `Id<T>.as_usize()`, with the node type
/// tracked via `PhantomData<T>`.
/// Used for densely populated metadata associated with nodes of type `T`.
#[derive(Debug)]
pub struct MetaVec<T, M> {
    data: Vec<M>,
    _marker: PhantomData<T>,
}

impl<T, M> MetaVec<T, M> {
    pub fn new(cp: usize) -> Self
    where
        M: Default + Clone,
    {
        let data = vec![M::default(); cp];

        Self {
            data,
            _marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn into_iter(self) -> vec::IntoIter<M> {
        self.data.into_iter()
    }

    pub fn iter(&self) -> slice::Iter<'_, M> {
        self.data.iter()
    }

    pub fn iter_mut(&mut self) -> slice::IterMut<'_, M> {
        self.data.iter_mut()
    }

    pub fn as_slice(&self) -> &[M] {
        self.data.as_slice()
    }

    pub fn as_mut_slice(&mut self) -> &mut [M] {
        self.data.as_mut_slice()
    }
}

impl<T, M> Col<T> for MetaVec<T, M> {
    type Column = Self;
    type Ids<'a>
        = IdIter<T>
    where
        Self: 'a;

    fn col(&self) -> &Self::Column {
        self
    }

    fn col_mut(&mut self) -> &mut Self::Column {
        self
    }

    fn ids<'a>(&'a self) -> Self::Ids<'a> {
        IdIter::new(0, self.data.len() as u32)
    }
}

impl<T, M> Get<T> for MetaVec<T, M> {
    type Item = M;

    fn get(&self, id: Id<T>) -> &M {
        &self.data[id.as_usize()]
    }

    fn get_mut(&mut self, id: Id<T>) -> &mut M {
        &mut self.data[id.as_usize()]
    }
}

impl<T, M> Index<Id<T>> for MetaVec<T, M> {
    type Output = M;

    fn index(&self, id: Id<T>) -> &Self::Output {
        &self.data[id.as_usize()]
    }
}

impl<T, M> IndexMut<Id<T>> for MetaVec<T, M> {
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        &mut self.data[id.as_usize()]
    }
}

impl<T, M> Clone for MetaVec<T, M>
where
    M: Clone,
{
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T, M> Extend<(Id<T>, M)> for MetaVec<T, M> {
    fn extend<I: IntoIterator<Item = (Id<T>, M)>>(&mut self, iter: I) {
        for (id, value) in iter {
            self.data[id.as_usize()] = value;
        }
    }
}

/// A simple `HashMap` indexed by `Id<T>`.
/// Used for sparsely populated metadata associated with nodes of type `T`.
#[derive(Debug)]
pub struct MetaMap<T, M>(HashMap<Id<T>, M>);

impl<T, M> MetaMap<T, M> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn remove(&mut self, id: Id<T>) -> Option<M> {
        self.0.remove(&id)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> hash_map::Iter<'_, Id<T>, M> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> hash_map::IterMut<'_, Id<T>, M> {
        self.0.iter_mut()
    }

    pub fn into_iter(self) -> hash_map::IntoIter<Id<T>, M> {
        self.0.into_iter()
    }
}

impl<T, M> Clone for MetaMap<T, M>
where
    M: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T, M> Extend<(Id<T>, M)> for MetaMap<T, M> {
    fn extend<I: IntoIterator<Item = (Id<T>, M)>>(&mut self, iter: I) {
        self.0.extend(iter);
    }
}

impl<T, M> Col<T> for MetaMap<T, M> {
    type Column = Self;
    type Ids<'a>
        = Copied<hash_map::Keys<'a, Id<T>, M>>
    where
        Self: 'a;

    fn col(&self) -> &Self::Column {
        self
    }

    fn col_mut(&mut self) -> &mut Self::Column {
        self
    }

    fn ids<'a>(&'a self) -> Self::Ids<'a> {
        self.0.keys().copied()
    }
}

impl<T, M> GetOpt<T> for MetaMap<T, M> {
    type Item = M;

    fn get_opt(&self, id: Id<T>) -> Option<&M> {
        self.0.get(&id)
    }

    fn get_opt_mut(&mut self, id: Id<T>) -> Option<&mut M> {
        self.0.get_mut(&id)
    }

    fn set(&mut self, id: Id<T>, value: M) -> Option<M> {
        self.0.insert(id, value)
    }
}
