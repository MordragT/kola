use kola_print::prelude::*;

use crate::{
    id::NodeId,
    node::InnerNode,
    tree::{NodeContainer, Tree},
};

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

    pub fn with(mut self, decorator: impl Decorator + 'static) -> Self {
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
        let root = self.tree.root_id();
        self.decorate(root, arena)
    }
}
