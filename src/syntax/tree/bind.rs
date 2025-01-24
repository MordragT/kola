use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Metadata, Name, Node, NodeId, Tree};
use crate::syntax::print::prelude::*;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Let {
    pub name: NodeId<Name>,
    pub value: NodeId<Expr>,
    pub inside: NodeId<Expr>,
}

impl<M> Printable<Tree<M>> for Let
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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
