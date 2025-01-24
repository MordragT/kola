use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Ident, Metadata, Node, NodeId, Pat, Tree};
use crate::syntax::print::prelude::*;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct If {
    pub predicate: NodeId<Expr>,
    pub then: NodeId<Expr>,
    pub or: NodeId<Expr>,
}

impl<M> Printable<Tree<M>> for If
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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

impl<M> Printable<Tree<M>> for Branch
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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

impl<M> Printable<Tree<M>> for Case
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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
