use crate::{id::Id, slice::SliceId};

/// Identity & Traversal: Knowing which IDs exist in a container.
pub trait RelationIds<K> {
    type Ids<'a>: Iterator<Item = Id<K>> + 'a
    where
        Self: 'a;

    fn ids(&self) -> Self::Ids<'_>;
}

/// Sparse/Optional Read Access: Map `Id<K>` to `Option<&V>`.
pub trait PartialRelation<K, V>: RelationIds<K> {
    fn get_opt(&self, id: Id<K>) -> Option<&V>;
}

/// Dense/Guaranteed Read Access: Map `Id<K>` directly to `&V` without branching,
/// with support for high-performance direct row iteration.
pub trait TotalRelation<K, V>: PartialRelation<K, V> {
    fn get(&self, id: Id<K>) -> &V;

    type Rows<'a>: Iterator<Item = (Id<K>, &'a V)> + 'a
    where
        Self: 'a,
        V: 'a;

    fn rows(&self) -> Self::Rows<'_>;
}

/// Dense/Guaranteed Mutable Access: Map `Id<K>` to `&mut V` for in-place modifications,
/// with support for high-performance direct mutable row iteration.
pub trait TotalRelationMut<K, V>: TotalRelation<K, V> {
    fn get_mut(&mut self, id: Id<K>) -> &mut V;

    type RowsMut<'a>: Iterator<Item = (Id<K>, &'a mut V)> + 'a
    where
        Self: 'a,
        V: 'a;

    fn rows_mut(&mut self) -> Self::RowsMut<'_>;
}

/// Allocation: Appending values to storage, returning a typed ID.
pub trait Allocate<K> {
    type Value;

    fn alloc(&mut self, value: Self::Value) -> Id<K>;
}

/// Slice Relation: Resolves a `SliceId<T>` to a physical slice of `Id<T>`.
pub trait SliceRelation<T> {
    fn get_slice(&self, id: SliceId<T>) -> &[Id<T>];
}
