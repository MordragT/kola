use std::marker::PhantomData;

use crate::{node::NodeStorage, tree::TreeView};

/// Columnar access to a `Vec<Item>` inside a storage.
pub trait Col<T> {
    type Item;
    fn vec(&self) -> &Vec<Self::Item>;
    fn vec_mut(&mut self) -> &mut Vec<Self::Item>;

    fn get(&self, id: Id<T>) -> &Self::Item {
        &self.vec()[id.as_usize()]
    }

    fn get_mut(&mut self, id: Id<T>) -> &mut Self::Item {
        &mut self.vec_mut()[id.as_usize()]
    }

    fn set(&mut self, id: Id<T>, value: Self::Item) -> Self::Item {
        std::mem::replace(self.get_mut(id), value)
    }

    fn len(&self) -> usize {
        self.vec().len()
    }
}

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

    pub fn get<'a>(self, storage: &'a impl TreeView) -> &'a T
    where
        NodeStorage: Col<T, Item = T>,
    {
        storage.nodes().get(self)
    }
}
