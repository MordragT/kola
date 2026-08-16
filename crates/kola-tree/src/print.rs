use std::convert::identity;

use kola_interner::StrInterner;
use kola_print::prelude::*;
use kola_relations::{Id, SliceId, SliceRelation, TotalRelation};
use kola_span::Loc;

use crate::{
    node::AnyId,
    tree::{TotalSpan, Tree},
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

pub struct TreePrinter<'a, M = Loc> {
    pub tree: &'a Tree<M>,
    pub interner: &'a StrInterner,
    pub decorators: Decorators<'a>,
    pub print_spans: bool,
}

impl<'a, M> Clone for TreePrinter<'a, M> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, M> Copy for TreePrinter<'a, M> {}

impl<'a, M: 'static> TreePrinter<'a, M> {
    pub fn new(
        tree: &'a Tree<M>,
        interner: &'a StrInterner,
        decorators: Decorators<'a>,
        print_spans: bool,
    ) -> Self {
        Self {
            tree,
            interner,
            decorators,
            print_spans,
        }
    }
}

impl<'a, T, M> Notate<'a, Id<T>> for TreePrinter<'a, M>
where
    TreePrinter<'a, M>: Notate<'a, T>,
    T: 'static,
    Tree<M>: TotalRelation<T, T> + TotalSpan<T, M>,
    M: 'static + std::fmt::Display,
    AnyId: From<Id<T>>,
{
    fn notate(self, value: &Id<T>, arena: &'a Bump) -> Notation<'a> {
        let node = self.tree.get(*value);
        let mut notation = self.notate(node, arena);
        if self.print_spans {
            let span: &M = self.tree.get_span(*value);
            let head = span.display_in(arena);

            let single = arena.just(' ').then(notation.clone().flatten(arena), arena);
            let multi = arena.newline().then(notation, arena);

            notation = head.then(single.or(multi, arena), arena);
        }
        self.decorators.decorate(notation, (*value).into(), arena)
    }
}

impl<'a, T, M> Notate<'a, SliceId<T>> for TreePrinter<'a, M>
where
    TreePrinter<'a, M>: Notate<'a, Id<T>>,
    T: 'static,
    Tree<M>: SliceRelation<T>,
{
    fn notate(self, value: &SliceId<T>, arena: &'a Bump) -> Notation<'a> {
        let ids = self.tree.get_slice(*value);
        let notations = ids
            .iter()
            .map(|id| self.notate(id, arena))
            .collect_in::<BumpVec<_>>(arena);

        let single = notations
            .clone()
            .concat_by(arena.just(' '), arena)
            .flatten(arena);
        let multi = notations.concat_by(arena.newline(), arena).indent(arena);
        single.or(multi, arena)
    }
}

impl<'a, T, M> Notate<'a, Option<T>> for TreePrinter<'a, M>
where
    TreePrinter<'a, M>: Notate<'a, T>,
    T: 'a,
{
    fn notate(self, value: &Option<T>, arena: &'a Bump) -> Notation<'a> {
        match value {
            Some(v) => self.notate(v, arena),
            None => arena.empty(),
        }
    }
}
