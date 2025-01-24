use std::fmt;

use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Metadata, Node, NodeId, Tree};
use crate::syntax::print::prelude::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "Neg"),
            Self::Not => write!(f, "Not"),
        }
    }
}

impl<M> Printable<Tree<M>> for UnaryOp {
    fn notate<'a>(&'a self, _with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        self.display_in(arena)
    }
}

impl TryFrom<Node> for UnaryOp {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::UnaryOp(u) => Ok(u),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Unary {
    pub op: NodeId<UnaryOp>,
    pub target: NodeId<Expr>,
}

impl<M> Printable<Tree<M>> for Unary
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        let Self { op, target } = self;

        let head = "Unary".blue().display_in(arena);

        let op = op.notate(with, arena);
        let target = target.notate(with, arena);

        let single = [
            arena.just(' '),
            op.clone(),
            arena.just(' '),
            target.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [arena.newline(), op, arena.newline(), target]
            .concat_in(arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for Unary {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Unary(u) => Ok(u),
            _ => Err(()),
        }
    }
}
