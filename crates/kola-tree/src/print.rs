use kola_print::prelude::*;
use kola_utils::TryAsRef;

use crate::{
    id::Id,
    node::Node,
    tree::{Tree, TreeView},
};

// This is somewhat hacky I use NodeId<()> and then only the Metadata get function
// so that there is no type safety for the NodeId and what Node I get,
// but it allows to make the Decorator trait dyn safe.

pub trait Decorator {
    fn decorate<'a>(
        &'a self,
        notation: Notation<'a>,
        with: Id<()>,
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

    pub fn decorate<'a, T>(&'a self, id: Id<T>, arena: &'a Bump) -> Notation<'a>
    where
        Node: TryAsRef<T>,
        T: Printable<Self> + 'a,
    {
        let mut notation = self.tree.node::<T>(id).notate(&self, arena);

        for decorator in &self.decorators {
            notation = decorator.decorate(notation, id.cast(), arena);
        }

        notation
    }

    pub fn render_at<T>(&self, id: Id<T>, options: PrintOptions) -> String
    where
        Node: TryAsRef<T>,
        T: Printable<Self>,
    {
        let arena = Bump::new();
        let notation = self.decorate(id, &arena);

        let mut output = String::new();
        let mut printer = Printer::new(&notation, options, &arena);
        printer.print(&mut output, &arena).unwrap();
        output
    }

    pub fn print_at<T>(&self, id: Id<T>, options: PrintOptions)
    where
        Node: TryAsRef<T>,
        T: Printable<Self>,
    {
        let output = self.render_at(id, options);
        println!("{}", output);
    }
}

impl Printable<()> for TreePrinter {
    fn notate<'a>(&'a self, _with: &'a (), arena: &'a Bump) -> Notation<'a> {
        let root = self.tree.root_id();
        self.decorate(root, arena)
    }
}
