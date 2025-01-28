use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Ident, InnerNode, Pat};
use crate::{Attached, Meta, Node, NodeId, Phase, print::TreePrinter};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct If {
    pub predicate: NodeId<Expr>,
    pub then: NodeId<Expr>,
    pub or: NodeId<Expr>,
}

impl InnerNode for If {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::If(i) => Some(i),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::If(i) => Some(i),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for If {
    type Meta = P::If;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::If(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::If(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::If(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for If {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            predicate,
            then,
            or,
        } = self;

        let head = "Let".blue().display_in(arena);

        let predicate = predicate.notate(with, arena);
        let then = then.notate(with, arena);
        let or = or.notate(with, arena);

        let single = [
            arena.notate(" predicate = "),
            predicate.clone().flatten(arena),
            arena.notate(", then = "),
            then.clone().flatten(arena),
            arena.notate(", or = "),
            or.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("predicate = "),
            predicate,
            arena.newline(),
            arena.notate("then = "),
            then,
            arena.newline(),
            arena.notate("or = "),
            or,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Branch {
    pub pat: NodeId<Pat>,
    pub matches: NodeId<Expr>,
}

impl InnerNode for Branch {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Branch(b) => Some(b),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Branch(b) => Some(b),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Branch {
    type Meta = P::Branch;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Branch(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Branch(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Branch(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for Branch {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { pat, matches } = self;

        let head = "Branch".blue().display_in(arena);

        let pat = pat.notate(with, arena);
        let matches = matches.notate(with, arena);

        let single = [
            arena.notate(" pat = "),
            pat.clone().flatten(arena),
            arena.notate(", matches = "),
            matches.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("pat = "),
            pat,
            arena.newline(),
            arena.notate("matches = "),
            matches,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for Branch {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Branch(b) => Ok(b),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Case {
    pub source: NodeId<Ident>,
    pub branches: Vec<NodeId<Branch>>,
}

impl InnerNode for Case {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Case(c) => Some(c),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Case(c) => Some(c),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Case {
    type Meta = P::Case;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Case(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Case(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Case(m) => Some(m),
            _ => None,
        }
    }
}

impl Printable<TreePrinter> for Case {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { source, branches } = self;

        let head = "Case".blue().display_in(arena);

        let source = source.notate(with, arena);
        let branches = branches.gather(with, arena).concat_in(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", branches = "),
            branches.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source.clone(),
            arena.newline(),
            arena.notate("branches = "),
            branches,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for Case {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Case(c) => Ok(c),
            _ => Err(()),
        }
    }
}
