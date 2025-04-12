use std::fmt;

use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::Expr;
use crate::{
    id::NodeId,
    print::TreePrinter,
    tree::{NodeContainer, TreeBuilder},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

impl Printable<TreePrinter> for BinaryOp {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        self.display_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Binary {
    pub op: NodeId<BinaryOp>,
    pub left: NodeId<Expr>,
    pub right: NodeId<Expr>,
}

impl Binary {
    pub fn new_in(
        op: BinaryOp,
        left: Expr,
        right: Expr,
        builder: &mut TreeBuilder,
    ) -> NodeId<Self> {
        let op = builder.insert(op);
        let left = builder.insert(left);
        let right = builder.insert(right);

        builder.insert(Self { op, left, right })
    }

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
