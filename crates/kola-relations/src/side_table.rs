use crate::relation::{
    PartialRelation, PartialRelationMut, RelationIds, TotalRelation, TotalRelationMut,
};
use crate::{Id, IdIter};
use std::collections::HashMap;
use std::marker::PhantomData;

/// A flat, contiguous vector indexed by `Id<T>.as_usize()`.
/// Used for densely populated metadata associated with nodes of type `T`.
#[derive(Debug, Clone)]
pub struct SideVec<T, M> {
    data: Vec<M>,
    _marker: PhantomData<T>,
}

impl<T, M> SideVec<T, M> {
    #[inline]
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn from_vec(data: Vec<M>) -> Self {
        Self {
            data,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    #[inline]
    pub fn push(&mut self, value: M) {
        self.data.push(value);
    }

    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
    }

    #[inline]
    pub fn truncate(&mut self, len: usize) {
        self.data.truncate(len);
    }

    #[inline]
    pub fn as_slice(&self) -> &[M] {
        self.data.as_slice()
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [M] {
        self.data.as_mut_slice()
    }
}

impl<T, M> Default for SideVec<T, M> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: 'static, M: 'static> RelationIds<T> for SideVec<T, M> {
    type Ids<'a>
        = IdIter<T>
    where
        Self: 'a;

    #[inline]
    fn ids(&self) -> Self::Ids<'_> {
        IdIter::new(0, self.data.len() as u32)
    }
}

impl<T: 'static, M: 'static> PartialRelation<T, M> for SideVec<T, M> {
    #[inline]
    fn get_opt(&self, id: Id<T>) -> Option<&M> {
        self.data.as_slice().get(id.as_usize())
    }
}

impl<T: 'static, M: 'static> TotalRelation<T, M> for SideVec<T, M> {
    #[inline]
    fn get(&self, id: Id<T>) -> &M {
        &self.data[id.as_usize()]
    }

    type Rows<'a>
        = std::iter::Zip<IdIter<T>, std::slice::Iter<'a, M>>
    where
        Self: 'a,
        M: 'a;

    #[inline]
    fn rows(&self) -> Self::Rows<'_> {
        self.ids().zip(self.data.iter())
    }
}

impl<T: 'static, M: 'static> TotalRelationMut<T, M> for SideVec<T, M> {
    #[inline]
    fn get_mut(&mut self, id: Id<T>) -> &mut M {
        &mut self.data[id.as_usize()]
    }

    type RowsMut<'a>
        = std::iter::Zip<IdIter<T>, std::slice::IterMut<'a, M>>
    where
        Self: 'a,
        M: 'a;

    #[inline]
    fn rows_mut(&mut self) -> Self::RowsMut<'_> {
        self.ids().zip(self.data.iter_mut())
    }
}

/// A flat, sparse map indexed by `Id<T>`.
/// Used for sparsely populated metadata associated with nodes of type `T`.
#[derive(Debug, Clone)]
pub struct SideMap<T, M>(HashMap<Id<T>, M>);

impl<T, M> SideMap<T, M> {
    #[inline]
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self(HashMap::with_capacity(capacity))
    }

    #[inline]
    pub fn from_map(map: HashMap<Id<T>, M>) -> Self {
        Self(map)
    }

    #[inline]
    pub fn insert(&mut self, id: Id<T>, value: M) -> Option<M> {
        self.0.insert(id, value)
    }

    #[inline]
    pub fn remove(&mut self, id: Id<T>) -> Option<M> {
        self.0.remove(&id)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn clear(&mut self) {
        self.0.clear();
    }
}

impl<T, M> Default for SideMap<T, M> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: 'static, M: 'static> RelationIds<T> for SideMap<T, M> {
    type Ids<'a>
        = std::iter::Copied<std::collections::hash_map::Keys<'a, Id<T>, M>>
    where
        Self: 'a;

    #[inline]
    fn ids(&self) -> Self::Ids<'_> {
        self.0.keys().copied()
    }
}

impl<T: 'static, M: 'static> PartialRelation<T, M> for SideMap<T, M> {
    #[inline]
    fn get_opt(&self, id: Id<T>) -> Option<&M> {
        self.0.get(&id)
    }
}

impl<T: 'static, M: 'static> PartialRelationMut<T, M> for SideMap<T, M> {
    #[inline]
    fn get_mut(&mut self, id: Id<T>) -> Option<&mut M> {
        self.0.get_mut(&id)
    }
}
