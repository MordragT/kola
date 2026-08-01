use std::{
    collections::{HashMap, hash_map},
    iter::Copied,
    marker::PhantomData,
    ops::{Index, IndexMut},
    slice, vec,
};

use kola_subst::Substitutable;

use crate::{
    id::{Id, IdIter},
    node::{AnyId, StorageCheckpoint, UniversalStorage},
    query::{Col, Get, GetOpt},
};

/// A side-table where every column holds the same type `M`.
#[derive(Debug, Clone)]
pub struct NodeAttrs<M>(UniversalStorage<M>);

impl<M> NodeAttrs<M> {
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

    pub fn checkpoint(&self) -> NodeAttrsCheckpoint {
        NodeAttrsCheckpoint(self.0.checkpoint())
    }

    pub fn restore(&mut self, cp: &NodeAttrsCheckpoint) {
        self.0.restore(&cp.0);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NodeAttrsCheckpoint(StorageCheckpoint);

/// A simple `Vec<M>` indexed by `Id<T>.as_usize()`, with the node type
/// tracked via `PhantomData<T>`.
/// Used for densely populated metadata associated with nodes of type `T`.
#[derive(Debug)]
pub struct SideVec<T, M> {
    data: Vec<M>,
    _marker: PhantomData<T>,
}

impl<T, M> SideVec<T, M> {
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

impl<T, M> Col<T> for SideVec<T, M> {
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

impl<T, M> Get<T> for SideVec<T, M> {
    type Item = M;

    fn get(&self, id: Id<T>) -> &M {
        &self.data[id.as_usize()]
    }

    fn get_mut(&mut self, id: Id<T>) -> &mut M {
        &mut self.data[id.as_usize()]
    }
}

impl<T, M> Index<Id<T>> for SideVec<T, M> {
    type Output = M;

    fn index(&self, id: Id<T>) -> &Self::Output {
        &self.data[id.as_usize()]
    }
}

impl<T, M> IndexMut<Id<T>> for SideVec<T, M> {
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        &mut self.data[id.as_usize()]
    }
}

impl<T, M> Clone for SideVec<T, M>
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

impl<T, M> Extend<(Id<T>, M)> for SideVec<T, M> {
    fn extend<I: IntoIterator<Item = (Id<T>, M)>>(&mut self, iter: I) {
        for (id, value) in iter {
            self.data[id.as_usize()] = value;
        }
    }
}

impl<S, T, M> Substitutable<S> for SideVec<T, M>
where
    M: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result: Option<Self> = None;

        for (i, el) in self.iter().enumerate() {
            if let Some(el) = el.try_apply(s) {
                result.get_or_insert_with(|| self.clone()).as_mut_slice()[i] = el;
            }
        }

        result
    }

    fn apply_mut(&mut self, s: &mut S) {
        for el in self.iter_mut() {
            el.apply_mut(s);
        }
    }
}

/// A simple `HashMap` indexed by `Id<T>`.
/// Used for sparsely populated metadata associated with nodes of type `T`.
#[derive(Debug)]
pub struct SideMap<T, M>(HashMap<Id<T>, M>);

impl<T, M> SideMap<T, M> {
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

impl<T, M> Clone for SideMap<T, M>
where
    M: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T, M> Extend<(Id<T>, M)> for SideMap<T, M> {
    fn extend<I: IntoIterator<Item = (Id<T>, M)>>(&mut self, iter: I) {
        self.0.extend(iter);
    }
}

impl<T, M> Col<T> for SideMap<T, M> {
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

impl<T, M> GetOpt<T> for SideMap<T, M> {
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

impl<S, T, M> Substitutable<S> for SideMap<T, M>
where
    M: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result: Option<Self> = None;

        for (key, value) in self.iter() {
            if let Some(value) = value.try_apply(s) {
                result.get_or_insert_with(|| self.clone()).set(*key, value);
            }
        }

        result
    }

    fn apply_mut(&mut self, s: &mut S) {
        for (_id, value) in self.iter_mut() {
            value.apply_mut(s);
        }
    }
}

/// Defines a side-table struct: a collection of [`SideMap`]s, one per node
/// type, that can be queried via [`GetOpt`]/[`Col`] and substituted via
/// [`Substitutable`].
///
/// The generated struct is generic over the substitution type `S`, so it can
/// be shared between crates that use different substitutions (e.g. the
/// resolver's symbol substitution and the typer's type substitution).
///
/// # Example
///
/// ```
/// define_side_table!(NodeMap {
///     exprs: SideMap<node::Expr, MonoType>,
///     binds: SideMap<node::ValueBind, PolyType>,
/// });
/// ```
#[macro_export]
macro_rules! define_side_table {
    (
        $name:ident {
            $(
                $field:ident : SideMap<$node:ty, $value:ty>
            ),* $(,)?
        }
    ) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            $(
                pub $field: SideMap<$node, $value>,
            )*
        }

        $(
            impl $crate::query::GetOpt<$node> for $name {
                type Item = $value;

                fn get_opt(&self, id: $crate::id::Id<$node>) -> Option<&Self::Item> {
                    self.$field.get_opt(id)
                }

                fn get_opt_mut(&mut self, id: $crate::id::Id<$node>) -> Option<&mut Self::Item> {
                    self.$field.get_opt_mut(id)
                }

                fn set(&mut self, id: $crate::id::Id<$node>, value: Self::Item) -> Option<Self::Item> {
                    self.$field.set(id, value)
                }
            }

            impl $crate::query::Col<$node> for $name {
                type Column = SideMap<$node, $value>;
                type Ids<'a> = <SideMap<$node, $value> as $crate::query::Col<$node>>::Ids<'a>;

                #[inline]
                fn col(&self) -> &Self::Column {
                    &self.$field
                }

                #[inline]
                fn col_mut(&mut self) -> &mut Self::Column {
                    &mut self.$field
                }

                #[inline]
                fn ids<'a>(&'a self) -> Self::Ids<'a> {
                    self.$field.ids()
                }
            }
        )*

        impl<S> $crate::subst::Substitutable<S> for $name
        where
            $(
                SideMap<$node, $value>: $crate::subst::Substitutable<S> + Clone,
            )*
        {
            fn try_apply(&self, s: &mut S) -> Option<Self> {
                let mut changed = false;

                $(
                    let $field = match self.$field.try_apply(s) {
                        Some(x) => {
                            changed = true;
                            x
                        }
                        None => self.$field.clone(),
                    };
                )*

                changed.then_some(Self {
                    $(
                        $field,
                    )*
                })
            }
        }

        impl Default for $name {
            fn default() -> Self {
                Self {
                    $(
                        $field: SideMap::new(),
                    )*
                }
            }
        }

        impl $name {
            pub fn new() -> Self {
                Self::default()
            }

            /// Merge the entries of `other` into this table.
            pub fn extend(&mut self, other: Self) {
                $(
                    self.$field.extend(other.$field.into_iter());
                )*
            }
        }
    };
}
