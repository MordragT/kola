use std::fmt;

use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, InnerNode};
use crate::{Attached, Meta, Node, NodeId, Phase, print::TreePrinter};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl InnerNode for UnaryOp {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::UnaryOp(o) => Some(o),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::UnaryOp(o) => Some(o),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for UnaryOp {
    type Meta = P::UnaryOp;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::UnaryOp(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::UnaryOp(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::UnaryOp(m) => Some(m),
            _ => None,
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "Neg"),
            Self::Not => write!(f, "Not"),
        }
    }
}

impl Printable<TreePrinter> for UnaryOp {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
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

impl InnerNode for Unary {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Unary(u) => Some(u),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Unary(u) => Some(u),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Unary {
    type Meta = P::Unary;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Unary(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Unary(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Unary(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for Unary {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
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
