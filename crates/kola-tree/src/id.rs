use kola_utils::{convert::TryAsRef, define_unique_id};
use std::marker::PhantomData;

use crate::{
    meta::{MetaCast, MetaView, Phase},
    node::Node,
    tree::TreeView,
};

define_unique_id!(Id);

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

    pub(crate) fn cast<U>(self) -> Id<U> {
        Id {
            id: self.id,
            t: PhantomData,
        }
    }
}
