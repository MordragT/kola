use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{InnerNode, Node};
use crate::{
    Phase, Symbol,
    meta::{Attached, Meta},
    print::TreePrinter,
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Bool(bool),
    Num(f64),
    Char(char),
    Str(Symbol),
}

impl InnerNode for Literal {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Literal(l) => Some(l),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Literal(l) => Some(l),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Literal {
    type Meta = P::Literal;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Literal(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Literal(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Literal(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for Literal {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let kind = "Literal".purple().display_in(arena);

        let lit = match self {
            Self::Bool(b) => b.yellow().display_in(arena),
            Self::Num(n) => n.yellow().display_in(arena),
            Self::Char(c) => c.yellow().display_in(arena),
            Self::Str(s) => s.yellow().display_in(arena),
        }
        .enclose_by(arena.just('"'), arena);

        let single = arena.just(' ').then(lit.clone(), arena);
        let multi = arena.newline().then(lit, arena);

        kind.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for Literal {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Literal(l) => Ok(l),
            _ => Err(()),
        }
    }
}
