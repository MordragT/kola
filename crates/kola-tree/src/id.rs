use kola_print::prelude::*;
use kola_utils::convert::TryAsRef;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, hash::Hash, marker::PhantomData};

use crate::{
    meta::{MetaCast, MetaView, Phase},
    node::Node,
    print::TreePrinter,
    tree::TreeView,
};

#[derive(Debug, Serialize, Deserialize)]
pub struct Id<T> {
    id: u32,
    t: PhantomData<T>,
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            t: PhantomData,
        }
    }
}

impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl<T> Eq for Id<T> {}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
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

    pub fn as_usize(&self) -> usize {
        self.id as usize
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

    pub(crate) fn cast<U>(self) -> Id<U> {
        Id {
            id: self.id,
            t: PhantomData,
        }
    }
}

impl<T> Printable<TreePrinter> for Id<T>
where
    Node: TryAsRef<T>,
    T: Printable<TreePrinter>,
{
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        with.decorate(*self, arena)
    }
}
