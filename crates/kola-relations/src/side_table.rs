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

impl<T, M> SideMap<T, M> {
    /// Merge `other` into this map, overwriting on conflicts.
    #[inline]
    pub fn extend<I>(&mut self, other: I)
    where
        I: IntoIterator<Item = (Id<T>, M)>,
    {
        self.0.extend(other);
    }

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

/// Type-level dispatch for a per-node metadata table.
///
/// A side-table struct maps many node kinds to different value types, so the
/// generic `get_unchecked`/`set` accessors need a way to select the column from
/// the node type alone. `NodeMeta` is that selector: implemented once per
/// (struct, node kind) pair by [`define_side_table!`].
pub trait NodeMeta<S>: 'static {
    type Value: 'static;

    fn get(table: &S, id: Id<Self>) -> Option<&Self::Value>;
    fn get_mut(table: &mut S, id: Id<Self>) -> Option<&mut Self::Value>;
    fn set(table: &mut S, id: Id<Self>, value: Self::Value);
}

/// Declare a metadata table over `SideMap` columns.
///
/// Generates the struct, one [`NodeMeta`] impl per column, and the generic
/// `get_unchecked` / `get_unchecked_mut` / `set` / `extend` accessors.
#[macro_export]
macro_rules! define_side_table {
    (
        $(#[$meta:meta])*
        $name:ident {
            $( $field:ident : Self:: $node:ty => $value:ty ),* $(,)?
        }
    ) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Default)]
        pub struct $name {
            $( pub $field: $crate::side_table::SideMap<$node, $value>, )*
        }

        impl $name {
            #[inline]
            pub fn new() -> Self {
                Self::default()
            }

            /// Merge the entries of `other` into this table.
            pub fn extend(&mut self, other: Self) {
                $( self.$field.extend(other.$field.into_iter()); )*
            }

            #[inline]
            pub fn get_opt<T: $crate::side_table::NodeMeta<Self>>(
                &self,
                id: $crate::Id<T>,
            ) -> Option<&T::Value> {
                T::get(self, id)
            }

            #[inline]
            pub fn set<T: $crate::side_table::NodeMeta<Self>>(&mut self, id: $crate::Id<T>, value: T::Value) {
                T::set(self, id, value)
            }

            #[inline]
            #[track_caller]
            pub fn get_unchecked<T: $crate::side_table::NodeMeta<Self>>(
                &self,
                id: $crate::Id<T>,
            ) -> &T::Value {
                T::get(self, id).expect("node metadata missing")
            }

            #[inline]
            #[track_caller]
            pub fn get_unchecked_mut<T: $crate::side_table::NodeMeta<Self>>(
                &mut self,
                id: $crate::Id<T>,
            ) -> &mut T::Value {
                T::get_mut(self, id).expect("node metadata missing")
            }
        }

        $(
            impl $crate::relation::PartialRelation<$node, $value> for $name {
                #[inline]
                fn get_opt(&self, id: $crate::Id<$node>) -> Option<&$value> {
                    $crate::relation::PartialRelation::<$node, $value>::get_opt(&self.$field, id)
                }
            }

            impl $crate::relation::PartialRelationMut<$node, $value> for $name {
                #[inline]
                fn get_mut(&mut self, id: $crate::Id<$node>) -> Option<&mut $value> {
                    $crate::relation::PartialRelationMut::<$node, $value>::get_mut(&mut self.$field, id)
                }
            }
        )*

        $(
            impl $crate::side_table::NodeMeta<$name> for $node {
                type Value = $value;

                #[inline]
                fn get(table: &$name, id: $crate::Id<Self>) -> Option<&Self::Value> {
                    $crate::relation::PartialRelation::<$node, $value>::get_opt(&table.$field, id)
                }

                #[inline]
                fn get_mut(table: &mut $name, id: $crate::Id<Self>) -> Option<&mut Self::Value> {
                    $crate::relation::PartialRelationMut::<$node, $value>::get_mut(&mut table.$field, id)
                }

                #[inline]
                fn set(table: &mut $name, id: $crate::Id<Self>, value: Self::Value) {
                    $crate::side_table::SideMap::<$node, $value>::insert(&mut table.$field, id, value);
                }
            }
        )*
    };
}
