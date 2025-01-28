use std::ops::Deref;

use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{InnerNode, Node};
use crate::{
    Phase, Symbol,
    meta::{Attached, Meta},
    print::TreePrinter,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Ident(pub Symbol);

impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Deref for Ident {
    type Target = Symbol;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl InnerNode for Ident {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Ident(i) => Some(i),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Ident(i) => Some(i),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Ident {
    type Meta = P::Ident;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Ident(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Ident(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Ident(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for Ident {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "Ident".cyan().display_in(arena);

        let ident = self
            .0
            .yellow()
            .display_in(arena)
            .enclose_by(arena.just('"'), arena);

        let single = [arena.just(' '), ident.clone().flatten(arena)].concat_in(arena);
        let multi = [arena.newline(), ident].concat_in(arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for Ident {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Ident(i) => Ok(i),
            _ => Err(()),
        }
    }
}
