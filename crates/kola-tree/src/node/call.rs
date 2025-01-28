use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, InnerNode, Node};
use crate::{
    Phase,
    id::NodeId,
    meta::{Attached, Meta},
    print::TreePrinter,
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Call {
    pub func: NodeId<Expr>,
    pub arg: NodeId<Expr>,
}

impl InnerNode for Call {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Call(c) => Some(c),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Call(c) => Some(c),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Call {
    type Meta = P::Call;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Call(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Call(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Call(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for Call {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
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
