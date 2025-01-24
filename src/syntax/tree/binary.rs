use std::fmt;

use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Metadata, Node, NodeId, Tree};
use crate::syntax::print::prelude::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Less,
    Greater,
    LessEq,
    GreaterEq,
    // Logical
    And,
    Or,
    Xor,
    // Equality
    Eq,
    NotEq,
    // Record
    Merge,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "Add"),
            Self::Sub => write!(f, "Sub"),
            Self::Mul => write!(f, "Mul"),
            Self::Div => write!(f, "Div"),
            Self::Rem => write!(f, "Rem"),
            Self::Less => write!(f, "Less"),
            Self::Greater => write!(f, "Greater"),
            Self::LessEq => write!(f, "LessEq"),
            Self::GreaterEq => write!(f, "GreaterEq"),
            Self::And => write!(f, "And"),
            Self::Or => write!(f, "Or"),
            Self::Xor => write!(f, "Xor"),
            Self::Eq => write!(f, "Eq"),
            Self::NotEq => write!(f, "NotEq"),
            Self::Merge => write!(f, "Merge"),
        }
    }
}

impl<M> Printable<Tree<M>> for BinaryOp {
    fn notate<'a>(&'a self, _with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        self.display_in(arena)
    }
}

impl TryFrom<Node> for BinaryOp {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::BinaryOp(b) => Ok(b),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Binary {
    pub op: NodeId<BinaryOp>,
    pub left: NodeId<Expr>,
    pub right: NodeId<Expr>,
}

impl<M> Printable<Tree<M>> for Binary
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        let Self { op, left, right } = self;

        let head = "Binary".blue().display_in(arena);

        let left = left.notate(with, arena);
        let op = op.notate(with, arena);
        let right = right.notate(with, arena);

        let single = [
            arena.just(' '),
            left.clone().flatten(arena),
            arena.just(' '),
            op.clone().flatten(arena),
            arena.just(' '),
            right.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            left,
            arena.newline(),
            op,
            arena.newline(),
            right,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// impl Binary {
//     pub fn kind(&self) -> BinaryOpKind {
//         *self.op.inner()
//     }
// }

impl TryFrom<Node> for Binary {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Binary(b) => Ok(b),
            _ => Err(()),
        }
    }
}
