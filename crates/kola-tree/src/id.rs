use kola_print::prelude::*;
use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

use super::{Attached, Phase, Tree};
use crate::{InnerNode, MetaContainer, print::TreePrinter};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct NodeId<T> {
    id: u32,
    t: PhantomData<T>,
}

impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            t: PhantomData,
        }
    }
}

impl<T> Copy for NodeId<T> {}

impl<T> NodeId<T> {
    pub(super) fn new(id: u32) -> Self {
        Self { id, t: PhantomData }
    }

    pub fn as_usize(&self) -> usize {
        self.id as usize
    }

    pub fn get(self, tree: &Tree) -> &T
    where
        T: InnerNode,
    {
        tree.node(self)
    }

    // pub fn get_mut(self, tree: &mut Tree) -> &mut T
    // where
    //     T: InnerNode,
    // {
    //     tree.node_mut(self)
    // }

    pub fn meta<P>(self, metadata: &impl MetaContainer<P>) -> &T::Meta
    where
        P: Phase,
        T: Attached<P>,
    {
        metadata.meta(self)
    }

    pub fn meta_mut<P, M>(self, metadata: &mut impl MetaContainer<P>) -> &mut T::Meta
    where
        P: Phase,
        T: Attached<P>,
    {
        metadata.meta_mut(self)
    }

    pub(crate) fn cast<U>(self) -> NodeId<U> {
        NodeId {
            id: self.id,
            t: PhantomData,
        }
    }
}

impl<T> Printable<TreePrinter> for NodeId<T>
where
    T: InnerNode + Printable<TreePrinter>,
{
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        with.decorate(*self, arena)
    }
}
