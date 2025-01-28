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
pub struct Name(pub Symbol);

impl Name {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Deref for Name {
    type Target = Symbol;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl InnerNode for Name {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Name(n) => Some(n),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Name(n) => Some(n),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Name {
    type Meta = P::Name;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Name(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Name(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Name(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for Name {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "Name".cyan().display_in(arena);

        let name = self
            .0
            .yellow()
            .display_in(arena)
            .enclose_by(arena.just('"'), arena);

        let single = [arena.just(' '), name.clone()].concat_in(arena);
        let multi = [arena.newline(), name].concat_in(arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for Name {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Name(n) => Ok(n),
            _ => Err(()),
        }
    }
}
