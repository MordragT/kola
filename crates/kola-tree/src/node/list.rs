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
pub struct List {
    pub values: Vec<NodeId<Expr>>,
}

impl InnerNode for List {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::List(l) => Some(l),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::List(l) => Some(l),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for List {
    type Meta = P::List;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::List(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::List(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::List(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for List {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let kind = "List".blue().display_in(arena);

        let values = self.values.gather(with, arena);

        let single = values.clone().concat_map(
            |expr| arena.just(' ').then(expr.flatten(arena), arena),
            arena,
        );
        let multi = values
            .concat_map(|expr| arena.newline().then(expr, arena), arena)
            .indent(arena);

        kind.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for List {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::List(l) => Ok(l),
            _ => Err(()),
        }
    }
}
