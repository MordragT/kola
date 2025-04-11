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

impl Printable<TreePrinter> for UnaryOp {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        self.display_in(arena)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Unary {
    pub op: NodeId<UnaryOp>,
    pub target: NodeId<Expr>, // TODO rename target to arg or something better
}

impl Unary {
    pub fn new_in(op: UnaryOp, target: Expr, builder: &mut TreeBuilder) -> NodeId<Self> {
        let op = builder.insert(op);
        let target = builder.insert(target);

        builder.insert(Self { op, target })
    }

    pub fn op(&self, tree: &impl NodeContainer) -> UnaryOp {
        *self.op.get(tree)
    }

    pub fn target<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.target.get(tree)
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
