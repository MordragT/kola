use ecow::EcoString;

pub use binary::*;
pub use bind::*;
pub use call::*;
pub use cond::*;
pub use expr::*;
pub use func::*;
pub use ident::*;
pub use list::*;
pub use literal::*;
pub use name::*;
pub use node::*;
pub use pat::*;
pub use record::*;
pub use unary::*;

use crate::semantic::types::MonoType;

use super::{print::prelude::*, Span};

mod binary;
mod bind;
mod call;
mod cond;
mod expr;
mod func;
mod ident;
mod list;
mod literal;
mod name;
mod node;
mod pat;
mod record;
mod unary;

pub type SyntaxTree = Tree<Span>;

pub type Symbol = EcoString;

pub trait Metadata {
    fn span(&self) -> Span;
    fn ty(&self) -> Option<&MonoType>;
}

impl Metadata for Span {
    fn span(&self) -> Span {
        self.clone()
    }

    fn ty(&self) -> Option<&MonoType> {
        None
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Tree<M> {
    pub nodes: NodePool,
    pub meta: Vec<M>,
}

impl<M> Default for Tree<M> {
    fn default() -> Self {
        Self {
            nodes: NodePool::new(),
            meta: Vec::new(),
        }
    }
}

impl<M> Tree<M> {
    pub fn new() -> Self {
        Self::default()
    }

    // pub fn get<T>(&self, id: NodeId<T>) -> &T
    // where
    //     Node: TryAsRef<T>,
    // {
    //     self.nodes.get(id)
    // }

    // pub fn get_mut<T>(&mut self, id: NodeId<T>) -> &mut T
    // where
    //     Node: TryAsMut<T>,
    // {
    //     self.nodes.get_mut(id)
    // }

    pub fn insert<T>(&mut self, node: T, meta: M) -> NodeId<T>
    where
        T: Into<Node>,
    {
        self.meta.push(meta);
        self.nodes.insert(node)
    }

    pub fn map<F, U>(self, f: F) -> Tree<U>
    where
        F: FnMut(M) -> U,
    {
        let Self { nodes, meta } = self;

        let meta = meta.into_iter().map(f).collect();

        Tree { nodes, meta }
    }

    pub fn depth_first(&self) -> std::slice::Iter<'_, Node> {
        self.nodes.iter()
    }

    pub fn breadth_first(&self) -> std::iter::Rev<std::slice::Iter<'_, Node>> {
        self.nodes.iter().rev()
    }
}

impl<M> Printable<()> for Tree<M>
where
    M: Metadata,
{
    fn notate<'a>(&'a self, _with: &'a (), arena: &'a Bump) -> Notation<'a> {
        let root = self.nodes.root();
        let root = root.get(&self.nodes);
        root.notate(self, arena)
    }
}

pub trait TryAsMut<T> {
    fn try_as_mut(&mut self) -> Option<&mut T>;
}

pub trait TryAsRef<T> {
    fn try_as_ref(&self) -> Option<&T>;
}
