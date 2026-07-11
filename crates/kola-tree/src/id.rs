use std::{borrow::Borrow, marker::PhantomData};

use crate::node::{Column, NodeStorage};

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Id<T: ?Sized> {
    id: u32,
    t: std::marker::PhantomData<T>,
}

impl<T: ?Sized> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            t: std::marker::PhantomData,
        }
    }
}

impl<T: ?Sized> Copy for Id<T> {}

impl<T: ?Sized> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T: ?Sized> Eq for Id<T> {}

impl<T: ?Sized> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T: ?Sized> std::hash::Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T: ?Sized> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", std::any::type_name::<T>(), self.id)
    }
}

impl<T: ?Sized> Id<T> {
    pub fn as_usize(&self) -> usize {
        self.id as usize
    }

    pub fn id(&self) -> u32 {
        self.id
    }
}

impl<T> Id<T> {
    pub fn unchecked_from_usize(id: usize) -> Self {
        Self {
            id: id as u32,
            t: PhantomData,
        }
    }

    pub(crate) fn new(id: u32) -> Self {
        Self { id, t: PhantomData }
    }

    pub fn get<'a>(self, arena: &'a impl Borrow<NodeStorage>) -> &'a T
    where
        NodeStorage: Column<T, Item = T>,
    {
        arena.borrow().get(self)
    }
}

/// A typed range into the `slice_data` vector of a `Storage`.
///
/// `SliceId<T>` is a lightweight handle (8 bytes) to a contiguous range of
/// `Id<T>` values. This replaces `Vec<Id<T>>` in node structs.
#[derive(serde::Serialize, serde::Deserialize)]
pub struct SliceId<T: ?Sized> {
    start: u32,
    length: u32,
    t: PhantomData<T>,
}

impl<T: ?Sized> Clone for SliceId<T> {
    fn clone(&self) -> Self {
        Self {
            start: self.start,
            length: self.length,
            t: std::marker::PhantomData,
        }
    }
}

impl<T: ?Sized> Copy for SliceId<T> {}

impl<T: ?Sized> PartialEq for SliceId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.length == other.length
    }
}

impl<T: ?Sized> Eq for SliceId<T> {}

impl<T: ?Sized> PartialOrd for SliceId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for SliceId<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start
            .cmp(&other.start)
            .then(self.length.cmp(&other.length))
    }
}

impl<T: ?Sized> std::hash::Hash for SliceId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.length.hash(state);
    }
}

impl<T: ?Sized> std::fmt::Debug for SliceId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}[{}..{}]",
            std::any::type_name::<T>(),
            self.start,
            self.start + self.length
        )
    }
}

impl<T> SliceId<T> {
    pub(crate) fn new(start: u32, length: u32) -> Self {
        Self {
            start,
            length,
            t: PhantomData,
        }
    }

    pub fn empty() -> Self {
        Self {
            start: 0,
            length: 0,
            t: PhantomData,
        }
    }

    pub fn start(&self) -> usize {
        self.start as usize
    }

    pub fn end(&self) -> usize {
        (self.start + self.length) as usize
    }

    pub fn len(&self) -> usize {
        self.length as usize
    }

    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    pub fn get<'a>(self, arena: &'a impl Borrow<NodeStorage>) -> &'a [Id<T>]
    where
        NodeStorage: Column<T, Item = T>,
    {
        arena.borrow().get_slice(self)
    }

    pub fn iter<'a>(self, arena: &'a impl Borrow<NodeStorage>) -> impl Iterator<Item = Id<T>> + 'a
    where
        NodeStorage: Column<T, Item = T>,
        T: 'a,
    {
        let slice = self.get(arena);
        slice.iter().copied()
    }
}
