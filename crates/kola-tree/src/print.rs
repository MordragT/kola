use kola_print::prelude::*;

use crate::{InnerNode, Meta, MetaContainer, NodeId, Phase, Tree, meta::Metadata};

// This is somewhat hacky I use NodeId<()> and then only the Metadata get function
// so that there is no type safety for the NodeId and what Node I get,
// but it allows to make the Decorator trait dyn safe.

pub trait Decorator {
    fn decorate<'a>(
        &'a self,
        notation: Notation<'a>,
        with: NodeId<()>,
        arena: &'a Bump,
    ) -> Notation<'a>;
}

type MetaCallback<P> = Box<dyn for<'a> Fn(Notation<'a>, &'a Meta<P>, &'a Bump) -> Notation<'a>>;

pub struct MetaDecorator<P: Phase, C: MetaContainer<P>> {
    pub metadata: Metadata<P, C>,
    pub callback: MetaCallback<P>,
}

impl<'d, P, C> MetaDecorator<P, C>
where
    P: Phase,
    C: MetaContainer<P>,
{
    pub fn new(
        metadata: Metadata<P, C>,
        callback: impl for<'a> Fn(Notation<'a>, &'a Meta<P>, &'a Bump) -> Notation<'a> + 'static,
    ) -> Self {
        Self {
            metadata,
            callback: Box::new(callback),
        }
    }
}

impl<P, M> Decorator for MetaDecorator<P, M>
where
    P: Phase,
    M: MetaContainer<P>,
{
    fn decorate<'a>(
        &'a self,
        notation: Notation<'a>,
        with: NodeId<()>,
        arena: &'a Bump,
    ) -> Notation<'a> {
        let meta = self.metadata.get(with);
        (self.callback)(notation, meta, arena)
    }
}

pub struct TreePrinter {
    pub tree: Tree,
    pub decorators: Vec<Box<dyn Decorator>>,
}

impl TreePrinter {
    pub fn new(tree: &Tree) -> Self {
        Self {
            tree: tree.clone(),
            decorators: Vec::new(),
        }
    }

    pub fn add_decorator(mut self, decorator: impl Decorator + 'static) -> Self {
        self.decorators.push(Box::new(decorator));
        self
    }

    pub fn decorate<'a, T>(&'a self, id: NodeId<T>, arena: &'a Bump) -> Notation<'a>
    where
        T: InnerNode + Printable<Self> + 'a,
    {
        let mut notation = self.tree.node::<T>(id).notate(&self, arena);

        for decorator in &self.decorators {
            notation = decorator.decorate(notation, id.cast(), arena);
        }

        notation
    }
}

impl Printable<()> for TreePrinter {
    fn notate<'a>(&'a self, _with: &'a (), arena: &'a Bump) -> Notation<'a> {
        let root = self.tree.root();
        self.decorate(root, arena)
    }
}
