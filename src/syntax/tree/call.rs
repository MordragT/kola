use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Metadata, Node, NodeId, Tree};
use crate::syntax::print::prelude::*;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Call {
    pub func: NodeId<Expr>,
    pub arg: NodeId<Expr>,
}

impl<M> Printable<Tree<M>> for Call
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        let Self { func, arg } = self;

        let head = "Call".blue().display_in(arena);

        let func = func.notate(with, arena);
        let arg = arg.notate(with, arena);

        let single = [
            arena.notate(" func = "),
            func.clone().flatten(arena),
            arena.notate(", arg = "),
            arg.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("func = "),
            func,
            arena.newline(),
            arena.notate("arg = "),
            arg,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for Call {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Call(c) => Ok(c),
            _ => Err(()),
        }
    }
}
