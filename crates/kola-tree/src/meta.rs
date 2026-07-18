use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
    slice, vec,
};

use crate::{
    id::{Col, Id},
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
        UniversalStorage<M>: Col<T, Item = M>,
    {
        self.0.get(id)
    }

    pub fn set<T>(&mut self, id: Id<T>, value: M) -> M
    where
        UniversalStorage<M>: Col<T, Item = M>,
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

    pub fn get(&self, id: Id<T>) -> &M {
        &self.data[id.as_usize()]
    }

    pub fn get_mut(&mut self, id: Id<T>) -> &mut M {
        &mut self.data[id.as_usize()]
    }

    pub fn set(&mut self, id: Id<T>, value: M) {
        self.data[id.as_usize()] = value;
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

    pub fn vec(&self) -> &Vec<M> {
        &self.data
    }

    // TODO: This can break the invariant that the length of the vector is equal to the number of nodes of type T.
    // Currently necessary to implement Subtitute in the resolver crate
    pub fn vec_mut(&mut self) -> &mut Vec<M> {
        &mut self.data
    }
}

impl<T, M> Index<Id<T>> for MetaVec<T, M> {
    type Output = M;

    fn index(&self, id: Id<T>) -> &Self::Output {
        self.get(id)
    }
}

impl<T, M> IndexMut<Id<T>> for MetaVec<T, M> {
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        self.get_mut(id)
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
