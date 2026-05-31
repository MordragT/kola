use kola_utils::convert::TryAsRef;
use std::marker::PhantomData;

use crate::{
    meta::{MetaCast, MetaView, Phase},
    node::Node,
    tree::TreeView,
};

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

    pub fn get(self, tree: &impl TreeView) -> &T
    where
        Node: TryAsRef<T>,
    {
        tree.node(self)
    }

    pub fn meta<P>(self, metadata: &impl MetaView<P>) -> &T::Meta
    where
        P: Phase,
        T: MetaCast<P>,
    {
        metadata.meta(self)
    }

    pub fn meta_mut<P, M>(self, metadata: &mut impl MetaView<P>) -> &mut T::Meta
    where
        P: Phase,
        T: MetaCast<P>,
    {
        metadata.meta_mut(self)
    }

    // pub(crate) fn cast<U>(self) -> Id<U> {
    //     Id {
    //         id: self.id,
    //         t: PhantomData,
    //     }
    // }
}
