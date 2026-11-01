use crate::{id::Id, slice::SliceId};

/// Identity & Traversal: Knowing which IDs exist in a container.
pub trait RelationIds<K: 'static> {
    type Ids<'a>: Iterator<Item = Id<K>> + 'a
    where
        Self: 'a;

    fn ids(&self) -> Self::Ids<'_>;
}

/// Sparse/Optional Read Access: Map `Id<K>` to `Option<&V>`.
pub trait PartialRelation<K: 'static, V: 'static> {
    fn get_opt(&self, id: Id<K>) -> Option<&V>;
}

/// Sparse/Optional Mutable Access: Map `Id<K>` to `Option<&mut V>`.
pub trait PartialRelationMut<K: 'static, V: 'static>: PartialRelation<K, V> {
    fn get_mut(&mut self, id: Id<K>) -> Option<&mut V>;
}

/// Dense/Guaranteed Read Access: Map `Id<K>` directly to `&V` without branching,
/// with support for high-performance direct row iteration.
pub trait TotalRelation<K: 'static, V: 'static>: RelationIds<K> {
    fn get(&self, id: Id<K>) -> &V;

    type Rows<'a>: Iterator<Item = (Id<K>, &'a V)> + 'a
    where
        Self: 'a,
        V: 'a;

    fn rows(&self) -> Self::Rows<'_>;
}

/// Dense/Guaranteed Mutable Access: Map `Id<K>` to `&mut V` for in-place modifications,
/// with support for high-performance direct mutable row iteration.
pub trait TotalRelationMut<K: 'static, V: 'static>: TotalRelation<K, V> {
    fn get_mut(&mut self, id: Id<K>) -> &mut V;

    type RowsMut<'a>: Iterator<Item = (Id<K>, &'a mut V)> + 'a
    where
        Self: 'a,
        V: 'a;

    fn rows_mut(&mut self) -> Self::RowsMut<'_>;
}

/// Allocation: Appending values to storage, returning a typed ID.
pub trait Allocate<V: 'static> {
    type Key: 'static;

    fn alloc(&mut self, value: V) -> Id<Self::Key>;
}

/// Slice Relation: Resolves a `SliceId<T>` to a physical slice of `Id<T>`.
pub trait SliceRelation<T: 'static> {
    fn get_slice(&self, id: SliceId<T>) -> &[Id<T>];

    #[inline]
    fn id_at(&self, id: SliceId<T>, index: usize) -> Option<Id<T>> {
        self.get_slice(id).get(index).copied()
    }
}

/// Mutable slice relation: exposes the `Id<T>` backing storage.
pub trait SliceRelationMut<T: 'static>: SliceRelation<T> {
    fn get_slice_mut(&mut self, id: SliceId<T>) -> &mut [Id<T>];
}

// ==========================================
// Blanket Reference Implementations
// ==========================================

impl<T, K> RelationIds<K> for &T
where
    T: RelationIds<K> + ?Sized,
    K: 'static,
{
    type Ids<'a>
        = T::Ids<'a>
    where
        Self: 'a;

    #[inline]
    fn ids(&self) -> Self::Ids<'_> {
        (**self).ids()
    }
}

impl<T, K> RelationIds<K> for &mut T
where
    T: RelationIds<K> + ?Sized,
    K: 'static,
{
    type Ids<'a>
        = T::Ids<'a>
    where
        Self: 'a;

    #[inline]
    fn ids(&self) -> Self::Ids<'_> {
        (**self).ids()
    }
}

impl<T, K, V> PartialRelation<K, V> for &T
where
    T: PartialRelation<K, V> + ?Sized,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_opt(&self, id: Id<K>) -> Option<&V> {
        (**self).get_opt(id)
    }
}

impl<T, K, V> PartialRelation<K, V> for &mut T
where
    T: PartialRelation<K, V> + ?Sized,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_opt(&self, id: Id<K>) -> Option<&V> {
        (**self).get_opt(id)
    }
}

impl<T, K, V> PartialRelationMut<K, V> for &mut T
where
    T: PartialRelationMut<K, V> + ?Sized,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_mut(&mut self, id: Id<K>) -> Option<&mut V> {
        (**self).get_mut(id)
    }
}

impl<T, K, V> TotalRelation<K, V> for &T
where
    T: TotalRelation<K, V> + ?Sized,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get(&self, id: Id<K>) -> &V {
        (**self).get(id)
    }

    type Rows<'a>
        = T::Rows<'a>
    where
        Self: 'a,
        V: 'a;

    #[inline]
    fn rows(&self) -> Self::Rows<'_> {
        (**self).rows()
    }
}

impl<T, K, V> TotalRelation<K, V> for &mut T
where
    T: TotalRelation<K, V> + ?Sized,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get(&self, id: Id<K>) -> &V {
        (**self).get(id)
    }

    type Rows<'a>
        = T::Rows<'a>
    where
        Self: 'a,
        V: 'a;

    #[inline]
    fn rows(&self) -> Self::Rows<'_> {
        (**self).rows()
    }
}

impl<T, K, V> TotalRelationMut<K, V> for &mut T
where
    T: TotalRelationMut<K, V> + ?Sized,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_mut(&mut self, id: Id<K>) -> &mut V {
        (**self).get_mut(id)
    }

    type RowsMut<'a>
        = T::RowsMut<'a>
    where
        Self: 'a,
        V: 'a;

    #[inline]
    fn rows_mut(&mut self) -> Self::RowsMut<'_> {
        (**self).rows_mut()
    }
}

impl<T, V> Allocate<V> for &mut T
where
    T: Allocate<V> + ?Sized,
    V: 'static,
{
    type Key = T::Key;

    #[inline]
    fn alloc(&mut self, value: V) -> Id<Self::Key> {
        (**self).alloc(value)
    }
}

impl<T, U> SliceRelation<U> for &T
where
    T: SliceRelation<U> + ?Sized,
    U: 'static,
{
    #[inline]
    fn get_slice(&self, id: SliceId<U>) -> &[Id<U>] {
        (**self).get_slice(id)
    }
}

impl<T, U> SliceRelation<U> for &mut T
where
    T: SliceRelation<U> + ?Sized,
    U: 'static,
{
    #[inline]
    fn get_slice(&self, id: SliceId<U>) -> &[Id<U>] {
        (**self).get_slice(id)
    }
}

impl<T, U> SliceRelationMut<U> for &mut T
where
    T: SliceRelationMut<U> + ?Sized,
    U: 'static,
{
    #[inline]
    fn get_slice_mut(&mut self, id: SliceId<U>) -> &mut [Id<U>] {
        (**self).get_slice_mut(id)
    }
}

// ==========================================
// Blanket Standard Library Collections Implementations
// ==========================================

impl<K, V, S> PartialRelation<K, V> for std::collections::HashMap<Id<K>, V, S>
where
    S: std::hash::BuildHasher,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_opt(&self, id: Id<K>) -> Option<&V> {
        self.get(&id)
    }
}

impl<K, V> PartialRelation<K, V> for std::collections::BTreeMap<Id<K>, V>
where
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_opt(&self, id: Id<K>) -> Option<&V> {
        self.get(&id)
    }
}

impl<K, V, S> PartialRelationMut<K, V> for std::collections::HashMap<Id<K>, V, S>
where
    S: std::hash::BuildHasher,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_mut(&mut self, id: Id<K>) -> Option<&mut V> {
        self.get_mut(&id)
    }
}

impl<K, V> PartialRelationMut<K, V> for std::collections::BTreeMap<Id<K>, V>
where
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_mut(&mut self, id: Id<K>) -> Option<&mut V> {
        self.get_mut(&id)
    }
}

impl<K, V, S> RelationIds<K> for std::collections::HashMap<Id<K>, V, S>
where
    S: std::hash::BuildHasher,
    K: 'static,
    V: 'static,
{
    type Ids<'a>
        = std::iter::Copied<std::collections::hash_map::Keys<'a, Id<K>, V>>
    where
        Self: 'a;

    #[inline]
    fn ids(&self) -> Self::Ids<'_> {
        self.keys().copied()
    }
}

impl<K, V> RelationIds<K> for std::collections::BTreeMap<Id<K>, V>
where
    K: 'static,
    V: 'static,
{
    type Ids<'a>
        = std::iter::Copied<std::collections::btree_map::Keys<'a, Id<K>, V>>
    where
        Self: 'a;

    #[inline]
    fn ids(&self) -> Self::Ids<'_> {
        self.keys().copied()
    }
}

impl<K, V> RelationIds<K> for Vec<V>
where
    K: 'static,
    V: 'static,
{
    type Ids<'a>
        = crate::id::IdIter<K>
    where
        Self: 'a;

    #[inline]
    fn ids(&self) -> Self::Ids<'_> {
        crate::id::IdIter::new(0, self.len() as u32)
    }
}

impl<K, V> PartialRelation<K, V> for Vec<V>
where
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_opt(&self, id: Id<K>) -> Option<&V> {
        self.as_slice().get(id.as_usize())
    }
}

impl<K, V> TotalRelation<K, V> for Vec<V>
where
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get(&self, id: Id<K>) -> &V {
        &self[id.as_usize()]
    }

    type Rows<'a>
        = std::iter::Zip<crate::id::IdIter<K>, std::slice::Iter<'a, V>>
    where
        Self: 'a,
        V: 'a;

    #[inline]
    fn rows(&self) -> Self::Rows<'_> {
        self.ids().zip(self.iter())
    }
}

impl<K, V> TotalRelationMut<K, V> for Vec<V>
where
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_mut(&mut self, id: Id<K>) -> &mut V {
        &mut self[id.as_usize()]
    }

    type RowsMut<'a>
        = std::iter::Zip<crate::id::IdIter<K>, std::slice::IterMut<'a, V>>
    where
        Self: 'a,
        V: 'a;

    #[inline]
    fn rows_mut(&mut self) -> Self::RowsMut<'_> {
        self.ids().zip(self.iter_mut())
    }
}

impl<V> Allocate<V> for Vec<V>
where
    V: 'static,
{
    type Key = V;

    #[inline]
    fn alloc(&mut self, value: V) -> Id<V> {
        let id = Id::new(self.len() as u32);
        self.push(value);
        id
    }
}

// ==========================================
// Standard Tuple Implementations (2, 3, and 4-Tuples)
// ==========================================

macro_rules! impl_tuple_relations {
    ($first:ident $(, $rest:ident)+) => {
        impl<$first, $($rest,)+ K> RelationIds<K> for ($first, $($rest,)+)
        where
            $first: RelationIds<K>,
            K: 'static,
        {
            type Ids<'a>
                = $first::Ids<'a>
            where
                Self: 'a;

            #[inline]
            fn ids(&self) -> Self::Ids<'_> {
                self.0.ids()
            }
        }

        impl<$first, $($rest,)+ K, V> TotalRelation<K, V> for ($first, $($rest,)+)
        where
            $first: TotalRelation<K, V>,
            K: 'static,
            V: 'static,
        {
            #[inline]
            fn get(&self, id: Id<K>) -> &V {
                self.0.get(id)
            }

            type Rows<'a>
                = $first::Rows<'a>
            where
                Self: 'a,
                V: 'a;

            #[inline]
            fn rows(&self) -> Self::Rows<'_> {
                self.0.rows()
            }
        }

        impl<$first, $($rest,)+ K, V> TotalRelationMut<K, V> for ($first, $($rest,)+)
        where
            $first: TotalRelationMut<K, V>,
            K: 'static,
            V: 'static,
        {
            #[inline]
            fn get_mut(&mut self, id: Id<K>) -> &mut V {
                self.0.get_mut(id)
            }

            type RowsMut<'a>
                = $first::RowsMut<'a>
            where
                Self: 'a,
                V: 'a;

            #[inline]
            fn rows_mut(&mut self) -> Self::RowsMut<'_> {
                self.0.rows_mut()
            }
        }

        impl<$first, $($rest,)+ V> Allocate<V> for ($first, $($rest,)+)
        where
            $first: Allocate<V>,
            V: 'static,
        {
            type Key = $first::Key;

            #[inline]
            fn alloc(&mut self, value: V) -> Id<Self::Key> {
                self.0.alloc(value)
            }
        }

        impl<$first, $($rest,)+ T> SliceRelation<T> for ($first, $($rest,)+)
        where
            $first: SliceRelation<T>,
            T: 'static,
        {
            #[inline]
            fn get_slice(&self, id: SliceId<T>) -> &[Id<T>] {
                self.0.get_slice(id)
            }
        }
    };
}

impl_tuple_relations!(A, B);
impl_tuple_relations!(A, B, C);
impl_tuple_relations!(A, B, C, D);

// Partial relations layer tuples: metadata is looked up in the remaining
// fields, matching the `(data, local, base)` overlay convention.
impl<A, B, K, V> PartialRelation<K, V> for (A, B)
where
    B: PartialRelation<K, V>,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_opt(&self, id: Id<K>) -> Option<&V> {
        self.1.get_opt(id)
    }
}

impl<A, B, C, K, V> PartialRelation<K, V> for (A, B, C)
where
    B: PartialRelation<K, V>,
    C: PartialRelation<K, V>,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_opt(&self, id: Id<K>) -> Option<&V> {
        self.1.get_opt(id).or_else(|| self.2.get_opt(id))
    }
}

impl<A, B, C, D, K, V> PartialRelation<K, V> for (A, B, C, D)
where
    B: PartialRelation<K, V>,
    C: PartialRelation<K, V>,
    D: PartialRelation<K, V>,
    K: 'static,
    V: 'static,
{
    #[inline]
    fn get_opt(&self, id: Id<K>) -> Option<&V> {
        self.1
            .get_opt(id)
            .or_else(|| self.2.get_opt(id))
            .or_else(|| self.3.get_opt(id))
    }
}

// ==========================================
// Blanket Smart Pointer Implementations (Arc and Rc)
// ==========================================

macro_rules! impl_smart_pointer_relations {
    ($Ptr:ident, $module:ident) => {
        impl<T, K> RelationIds<K> for std::$module::$Ptr<T>
        where
            T: RelationIds<K> + ?Sized,
            K: 'static,
        {
            type Ids<'a>
                = T::Ids<'a>
            where
                Self: 'a;

            #[inline]
            fn ids(&self) -> Self::Ids<'_> {
                (**self).ids()
            }
        }

        impl<T, K, V> PartialRelation<K, V> for std::$module::$Ptr<T>
        where
            T: PartialRelation<K, V> + ?Sized,
            K: 'static,
            V: 'static,
        {
            #[inline]
            fn get_opt(&self, id: Id<K>) -> Option<&V> {
                (**self).get_opt(id)
            }
        }

        impl<T, K, V> TotalRelation<K, V> for std::$module::$Ptr<T>
        where
            T: TotalRelation<K, V> + ?Sized,
            K: 'static,
            V: 'static,
        {
            #[inline]
            fn get(&self, id: Id<K>) -> &V {
                (**self).get(id)
            }

            type Rows<'a>
                = T::Rows<'a>
            where
                Self: 'a,
                V: 'a;

            #[inline]
            fn rows(&self) -> Self::Rows<'_> {
                (**self).rows()
            }
        }

        // Cloning via `make_mut` is intentional: mutating through a shared
        // smart pointer clones the inner storage on demand (copy-on-write).
        impl<T, K, V> TotalRelationMut<K, V> for std::$module::$Ptr<T>
        where
            T: TotalRelationMut<K, V> + ?Sized,
            T: Clone,
            K: 'static,
            V: 'static,
        {
            #[inline]
            fn get_mut(&mut self, id: Id<K>) -> &mut V {
                std::$module::$Ptr::make_mut(self).get_mut(id)
            }

            type RowsMut<'a>
                = T::RowsMut<'a>
            where
                Self: 'a,
                V: 'a;

            #[inline]
            fn rows_mut(&mut self) -> Self::RowsMut<'_> {
                std::$module::$Ptr::make_mut(self).rows_mut()
            }
        }

        impl<T, V> Allocate<V> for std::$module::$Ptr<T>
        where
            T: Allocate<V> + ?Sized,
            T: Clone,
            V: 'static,
        {
            type Key = T::Key;

            #[inline]
            fn alloc(&mut self, value: V) -> Id<Self::Key> {
                std::$module::$Ptr::make_mut(self).alloc(value)
            }
        }

        impl<T, U> SliceRelation<U> for std::$module::$Ptr<T>
        where
            T: SliceRelation<U> + ?Sized,
            U: 'static,
        {
            #[inline]
            fn get_slice(&self, id: SliceId<U>) -> &[Id<U>] {
                (**self).get_slice(id)
            }
        }
    };
}

impl_smart_pointer_relations!(Arc, sync);
impl_smart_pointer_relations!(Rc, rc);
