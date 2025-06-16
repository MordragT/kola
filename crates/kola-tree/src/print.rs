use std::convert::identity;

use kola_print::prelude::*;
use kola_utils::{convert::TryAsRef, interner::StrInterner};

use crate::{
    id::Id,
    node::Node,
    tree::{Tree, TreeView},
};

pub trait Decorator<'a> {
    fn decorate(&self, notation: Notation<'a>, with: usize, arena: &'a Bump) -> Notation<'a>;
}

#[derive(Clone, Copy)]
pub struct Decorators<'a>([Option<&'a dyn Decorator<'a>>; 4]);

impl<'a> Decorators<'a> {
    pub fn new() -> Self {
        Self([None; 4])
    }

    pub fn with(mut self, decorator: &'a impl Decorator<'a>) -> Self {
        for slot in &mut self.0 {
            if slot.is_none() {
                *slot = Some(decorator);
                return self;
            }
        }

        panic!("No available slot for decorator");
    }
}

impl<'a> Decorator<'a> for Decorators<'a> {
    fn decorate(&self, notation: Notation<'a>, with: usize, arena: &'a Bump) -> Notation<'a> {
        self.0
            .into_iter()
            .filter_map(identity)
            .fold(notation, |n, d| d.decorate(n, with, arena))
    }
}

#[derive(Clone, Copy)]
pub struct TreePrinter<'a, T> {
    pub tree: &'a Tree,
    pub interner: &'a StrInterner,
    pub decorators: Decorators<'a>,
    pub value: T,
}

pub type NodePrinter<'a, T> = TreePrinter<'a, &'a T>;
pub type IdPrinter<'a, T> = TreePrinter<'a, Id<T>>;
pub type SlicePrinter<'a, T> = TreePrinter<'a, &'a [Id<T>]>;

impl<'a> IdPrinter<'a, crate::node::Module> {
    pub fn root(tree: &'a Tree, interner: &'a StrInterner, decorators: Decorators<'a>) -> Self {
        Self {
            tree,
            interner,
            decorators,
            value: tree.root_id(),
        }
    }
}

impl<'a, T> TreePrinter<'a, T> {
    pub fn new(
        tree: &'a Tree,
        interner: &'a StrInterner,
        decorators: Decorators<'a>,
        value: T,
    ) -> Self {
        Self {
            tree,
            interner,
            decorators,
            value,
        }
    }

    pub fn to<U>(self, value: U) -> TreePrinter<'a, U> {
        TreePrinter {
            tree: self.tree,
            interner: self.interner,
            decorators: self.decorators,
            value,
        }
    }

    pub fn to_node<U>(self, node: &'a U) -> NodePrinter<'a, U> {
        NodePrinter {
            tree: self.tree,
            interner: self.interner,
            decorators: self.decorators,
            value: node,
        }
    }

    pub fn to_id<U>(self, id: Id<U>) -> IdPrinter<'a, U> {
        IdPrinter {
            tree: self.tree,
            interner: self.interner,
            decorators: self.decorators,
            value: id,
        }
    }

    pub fn to_slice<U>(self, slice: &'a [Id<U>]) -> SlicePrinter<'a, U> {
        SlicePrinter {
            tree: self.tree,
            interner: self.interner,
            decorators: self.decorators,
            value: slice,
        }
    }
}

impl<'a, T> Notate<'a> for IdPrinter<'a, T>
where
    Node: TryAsRef<T>,
    NodePrinter<'a, T>: Notate<'a>,
    T: 'a,
{
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let node = self.tree.node(self.value);
        let notation = self.to(node).notate(arena);
        self.decorators
            .decorate(notation, self.value.as_usize(), arena)
    }
}

impl<'a, T> Gather<'a> for SlicePrinter<'a, T>
where
    Node: TryAsRef<T>,
    NodePrinter<'a, T>: Notate<'a>,
    T: 'a,
{
    fn gather(self, arena: &'a Bump) -> BumpVec<'a, Notation<'a>> {
        self.value
            .iter()
            .map(|id| self.to_id(*id).notate(arena))
            .collect_in(arena)
    }
}
