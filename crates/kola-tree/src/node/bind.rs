use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, InnerNode, Name, Node};
use crate::{
    Phase,
    id::NodeId,
    meta::{Attached, Meta},
    print::TreePrinter,
    tree::NodeContainer,
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Let {
    pub name: NodeId<Name>,
    pub value: NodeId<Expr>,
    pub inside: NodeId<Expr>,
}

impl Let {
    pub fn name<'a>(&self, tree: &'a impl NodeContainer) -> &'a Name {
        self.name.get(tree)
    }

    pub fn value<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.value.get(tree)
    }

    pub fn inside<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.inside.get(tree)
    }
}

impl InnerNode for Let {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Let(l) => Some(l),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Let(l) => Some(l),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Let {
    type Meta = P::Let;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Let(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Let(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Let(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for Let {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            name,
            value,
            inside,
        } = self;

        let head = "Let".blue().display_in(arena);

        let name = name.notate(with, arena);
        let value = value.notate(with, arena);
        let inside = inside.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
            arena.notate(", inside = "),
            inside.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("value = "),
            value,
            arena.newline(),
            arena.notate("inside = "),
            inside,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for Let {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Let(l) => Ok(l),
            _ => Err(()),
        }
    }
}
