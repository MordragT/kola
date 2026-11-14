use crate::relation::{
    Merge, PartialRelation, PartialRelationMut, PartialRelationSet, RelationIds, TotalRelation,
    TotalRelationMut,
};
use crate::{Id, IdIter};
use kola_subst::Substitutable;
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

/// A dense vector indexed by `Id<T>.as_usize()` whose slots may still be empty.
///
/// Unlike [`SideVec`], values can be inserted out of order: the backing storage
/// grows up to the requested id. Iteration only visits occupied slots.
#[derive(Debug, Clone)]
pub struct MaybeVec<T, M> {
    data: Vec<Option<M>>,
    _marker: PhantomData<T>,
}

impl<T, M> MaybeVec<T, M> {
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
            data: data.into_iter().map(Some).collect(),
            _marker: PhantomData,
        }
    }

    /// Number of slots, including holes.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
    }

    #[inline]
    pub fn truncate(&mut self, len: usize) {
        self.data.truncate(len);
    }

    /// Insert `value` at `id`, growing the storage with empty slots as needed.
    #[inline]
    pub fn insert(&mut self, id: Id<T>, value: M) -> Option<M> {
        let index = id.as_usize();
        if self.data.len() <= index {
            self.data.resize_with(index + 1, || None);
        }
        self.data[index].replace(value)
    }
}

impl<T, M> Default for MaybeVec<T, M> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T, M> Merge for MaybeVec<T, M> {
    fn merge(&mut self, other: Self) {
        for (index, slot) in other.data.into_iter().enumerate() {
            if let Some(value) = slot {
                self.insert(Id::new(index as u32), value);
            }
        }
    }
}

/// Ids of the occupied slots of a [`MaybeVec`], in ascending order.
pub struct MaybeVecIds<'a, T, M> {
    iter: std::iter::Enumerate<std::slice::Iter<'a, Option<M>>>,
    _marker: PhantomData<T>,
}

impl<'a, T, M: 'a> Iterator for MaybeVecIds<'a, T, M> {
    type Item = Id<T>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        for (index, slot) in self.iter.by_ref() {
            if slot.is_some() {
                return Some(Id::new(index as u32));
            }
        }
        None
    }
}

/// Occupied `(id, value)` rows of a [`MaybeVec`].
pub struct MaybeVecRows<'a, T, M> {
    iter: std::iter::Enumerate<std::slice::Iter<'a, Option<M>>>,
    _marker: PhantomData<T>,
}

impl<'a, T, M: 'a> Iterator for MaybeVecRows<'a, T, M> {
    type Item = (Id<T>, &'a M);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        for (index, slot) in self.iter.by_ref() {
            if let Some(value) = slot.as_ref() {
                return Some((Id::new(index as u32), value));
            }
        }
        None
    }
}

/// Occupied `(id, value)` rows of a [`MaybeVec`], mutably.
pub struct MaybeVecRowsMut<'a, T, M> {
    iter: std::iter::Enumerate<std::slice::IterMut<'a, Option<M>>>,
    _marker: PhantomData<T>,
}

impl<'a, T, M: 'a> Iterator for MaybeVecRowsMut<'a, T, M> {
    type Item = (Id<T>, &'a mut M);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        for (index, slot) in self.iter.by_ref() {
            if let Some(value) = slot.as_mut() {
                return Some((Id::new(index as u32), value));
            }
        }
        None
    }
}

impl<T: 'static, M: 'static> RelationIds<T> for MaybeVec<T, M> {
    type Ids<'a>
        = MaybeVecIds<'a, T, M>
    where
        Self: 'a;

    #[inline]
    fn ids(&self) -> Self::Ids<'_> {
        MaybeVecIds {
            iter: self.data.iter().enumerate(),
            _marker: PhantomData,
        }
    }
}

impl<T: 'static, M: 'static> PartialRelation<T, M> for MaybeVec<T, M> {
    #[inline]
    fn get_opt(&self, id: Id<T>) -> Option<&M> {
        self.data.as_slice().get(id.as_usize())?.as_ref()
    }
}

impl<T: 'static, M: 'static> PartialRelationMut<T, M> for MaybeVec<T, M> {
    #[inline]
    fn get_mut(&mut self, id: Id<T>) -> Option<&mut M> {
        self.data.as_mut_slice().get_mut(id.as_usize())?.as_mut()
    }
}

impl<T: 'static, M: 'static> PartialRelationSet<T, M> for MaybeVec<T, M> {
    #[inline]
    fn set(&mut self, id: Id<T>, value: M) {
        self.insert(id, value);
    }
}

impl<T: 'static, M: 'static> TotalRelation<T, M> for MaybeVec<T, M> {
    #[inline]
    fn get(&self, id: Id<T>) -> &M {
        self.data[id.as_usize()]
            .as_ref()
            .expect("maybe vec slot is empty")
    }

    type Rows<'a>
        = MaybeVecRows<'a, T, M>
    where
        Self: 'a,
        M: 'a;

    #[inline]
    fn rows(&self) -> Self::Rows<'_> {
        MaybeVecRows {
            iter: self.data.iter().enumerate(),
            _marker: PhantomData,
        }
    }
}

impl<T: 'static, M: 'static> TotalRelationMut<T, M> for MaybeVec<T, M> {
    #[inline]
    fn get_mut(&mut self, id: Id<T>) -> &mut M {
        self.data[id.as_usize()]
            .as_mut()
            .expect("maybe vec slot is empty")
    }

    type RowsMut<'a>
        = MaybeVecRowsMut<'a, T, M>
    where
        Self: 'a,
        M: 'a;

    #[inline]
    fn rows_mut(&mut self) -> Self::RowsMut<'_> {
        MaybeVecRowsMut {
            iter: self.data.iter_mut().enumerate(),
            _marker: PhantomData,
        }
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

impl<T, M> SideMap<T, M> {
    /// Iterate over `(id, value)` pairs.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (Id<T>, &M)> + '_ {
        self.0.iter().map(|(id, value)| (*id, value))
    }

    /// Iterate mutably over `(id, value)` pairs.
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Id<T>, &mut M)> + '_ {
        self.0.iter_mut().map(|(id, value)| (*id, value))
    }

    /// Consume the map, yielding `(id, value)` pairs.
    #[inline]
    pub fn into_iter(self) -> impl Iterator<Item = (Id<T>, M)> {
        self.0.into_iter()
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

impl<T: 'static, M: 'static> PartialRelationSet<T, M> for SideMap<T, M> {
    #[inline]
    fn set(&mut self, id: Id<T>, value: M) {
        self.0.insert(id, value);
    }
}

impl<T, M> Merge for SideMap<T, M> {
    #[inline]
    fn merge(&mut self, other: Self) {
        self.0.extend(other.0);
    }
}

impl<S, T, M> Substitutable<S> for SideVec<T, M>
where
    T: Clone,
    M: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result = None;

        for (index, value) in self.data.iter().enumerate() {
            if let Some(next) = value.try_apply(s) {
                result.get_or_insert_with(|| self.clone()).data[index] = next;
            }
        }

        result
    }

    fn apply_mut(&mut self, s: &mut S) {
        for value in &mut self.data {
            value.apply_mut(s);
        }
    }
}

impl<S, T, M> Substitutable<S> for MaybeVec<T, M>
where
    T: Clone,
    M: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result = None;

        for (index, slot) in self.data.iter().enumerate() {
            let Some(value) = slot else { continue };
            if let Some(next) = value.try_apply(s) {
                result.get_or_insert_with(|| self.clone()).data[index] = Some(next);
            }
        }

        result
    }

    fn apply_mut(&mut self, s: &mut S) {
        for value in self.data.iter_mut().flatten() {
            value.apply_mut(s);
        }
    }
}

impl<S, T, M> Substitutable<S> for SideMap<T, M>
where
    T: Clone,
    M: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result = None;

        for (id, value) in self.0.iter() {
            if let Some(next) = value.try_apply(s) {
                result
                    .get_or_insert_with(|| self.clone())
                    .0
                    .insert(*id, next);
            }
        }

        result
    }

    fn apply_mut(&mut self, s: &mut S) {
        for value in self.0.values_mut() {
            value.apply_mut(s);
        }
    }
}
