use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Ident, Metadata, Node, NodeId, Tree};
use crate::syntax::print::prelude::*;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Func {
    pub param: NodeId<Ident>,
    pub body: NodeId<Expr>,
}

impl<M> Printable<Tree<M>> for Func
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        let Self { param, body } = self;

        let head = "Func".blue().display_in(arena);

        let param = param.notate(with, arena);
        let body = body.notate(with, arena);

        let single = [
            arena.notate(" param = "),
            param.clone().flatten(arena),
            arena.notate(", body = "),
            body.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("param = "),
            param,
            arena.newline(),
            arena.notate("body = "),
            body,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for Func {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Func(f) => Ok(f),
            _ => Err(()),
        }
    }
}
