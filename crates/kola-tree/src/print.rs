use kola_print::prelude::*;
use kola_utils::{convert::TryAsRef, interner::StrInterner};

use crate::{
    id::Id,
    node::Node,
    tree::{Tree, TreeView},
};

pub trait Decorator {
    fn decorate<'a>(&'a self, notation: Notation<'a>, with: usize, arena: &'a Bump)
    -> Notation<'a>;
}

pub struct TreePrinter {
    pub tree: Tree,
    pub interner: StrInterner,
    pub decorators: Vec<Box<dyn Decorator>>,
}

impl TreePrinter {
    pub fn new(tree: Tree, interner: StrInterner) -> Self {
        Self {
            tree,
            interner,
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
            notation = decorator.decorate(notation, id.as_usize(), arena);
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
