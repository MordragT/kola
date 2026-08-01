use std::convert::identity;

use kola_print::prelude::*;
use kola_utils::interner::StrInterner;

use crate::{
    col::Get,
    id::Id,
    node::{AnyId, NodeStorage},
    slice::SliceId,
    tree::{Tree, TreeView},
};

pub trait Decorator<'a> {
    fn decorate(&self, notation: Notation<'a>, with: AnyId, arena: &'a Bump) -> Notation<'a>;
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
    fn decorate(&self, notation: Notation<'a>, with: AnyId, arena: &'a Bump) -> Notation<'a> {
        self.0
            .into_iter()
            .filter_map(identity)
            .fold(notation, |n, d| d.decorate(n, with, arena))
    }
}

#[derive(Clone, Copy)]
pub struct TreePrinter<'a> {
    pub tree: &'a Tree,
    pub interner: &'a StrInterner,
    pub decorators: Decorators<'a>,
}

impl<'a> TreePrinter<'a> {
    pub fn new(tree: &'a Tree, interner: &'a StrInterner, decorators: Decorators<'a>) -> Self {
        Self {
            tree,
            interner,
            decorators,
        }
    }
}

impl<'a, T> Notate<'a, Id<T>> for TreePrinter<'a>
where
    TreePrinter<'a>: Notate<'a, T>,
    T: 'a,
    NodeStorage: Get<T, Item = T>,
    AnyId: From<Id<T>>,
{
    fn notate(&self, value: &Id<T>, arena: &'a Bump) -> Notation<'a> {
        let node = self.tree.get(*value);
        let notation = self.notate(node, arena);
        self.decorators.decorate(notation, (*value).into(), arena)
    }
}

impl<'a, T> Notate<'a, SliceId<T>> for TreePrinter<'a>
where
    TreePrinter<'a>: Notate<'a, Id<T>>,
    T: 'a,
    NodeStorage: Get<T, Item = T>,
{
    fn notate(&self, value: &SliceId<T>, arena: &'a Bump) -> Notation<'a> {
        let ids = value.iter(self.tree);
        let notations = ids
            .map(|id| self.notate(&id, arena))
            .collect_in::<BumpVec<_>>(arena);

        let single = notations
            .clone()
            .concat_by(arena.just(' '), arena)
            .flatten(arena);
        let multi = notations.concat_by(arena.newline(), arena).indent(arena);
        single.or(multi, arena)
    }
}

impl<'a, T> Notate<'a, Option<T>> for TreePrinter<'a>
where
    TreePrinter<'a>: Notate<'a, T>,
    T: 'a,
{
    fn notate(&self, value: &Option<T>, arena: &'a Bump) -> Notation<'a> {
        match value {
            Some(v) => self.notate(v, arena),
            None => arena.empty(),
        }
    }
}
