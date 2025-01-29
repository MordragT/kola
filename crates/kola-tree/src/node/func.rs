use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Ident, InnerNode, Node};
use crate::{
    Phase,
    id::NodeId,
    meta::{Attached, Meta},
    print::TreePrinter,
    tree::NodeContainer,
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Func {
    pub param: NodeId<Ident>,
    pub body: NodeId<Expr>,
}

impl Func {
    pub fn param<'a>(&self, tree: &'a impl NodeContainer) -> &'a Ident {
        self.param.get(tree)
    }

    pub fn body<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.body.get(tree)
    }
}

impl InnerNode for Func {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Func(f) => Some(f),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Func(f) => Some(f),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Func {
    type Meta = P::Func;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Func(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Func(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Func(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for Func {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
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
