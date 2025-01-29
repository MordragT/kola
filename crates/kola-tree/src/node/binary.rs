use std::fmt;

use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, InnerNode, Node};
use crate::{
    Phase,
    id::NodeId,
    meta::{Attached, Meta},
    print::TreePrinter,
    tree::NodeContainer,
};

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

impl InnerNode for BinaryOp {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::BinaryOp(o) => Some(o),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::BinaryOp(o) => Some(o),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for BinaryOp {
    type Meta = P::BinaryOp;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::BinaryOp(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::BinaryOp(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::BinaryOp(m) => Some(m),
            _ => None,
        }
    }
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

impl Printable<TreePrinter> for BinaryOp {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
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

impl Binary {
    pub fn op(&self, tree: &impl NodeContainer) -> BinaryOp {
        *self.op.get(tree)
    }

    pub fn left<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.left.get(tree)
    }

    pub fn right<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.right.get(tree)
    }
}

impl InnerNode for Binary {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Binary(b) => Some(b),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Binary(b) => Some(b),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Binary {
    type Meta = P::Binary;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Binary(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Binary(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Binary(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for Binary {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
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

impl TryFrom<Node> for Binary {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Binary(b) => Ok(b),
            _ => Err(()),
        }
    }
}
