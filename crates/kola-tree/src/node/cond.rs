use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Ident, Pat};
use crate::{
    id::NodeId,
    print::TreePrinter,
    tree::{NodeContainer, TreeBuilder},
};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct If {
    pub predicate: NodeId<Expr>,
    pub then: NodeId<Expr>,
    pub or: NodeId<Expr>,
}

impl If {
    pub fn new_in(
        predicate: Expr,
        then: Expr,
        or: Expr,
        builder: &mut TreeBuilder,
    ) -> NodeId<Self> {
        let predicate = builder.insert(predicate);
        let then = builder.insert(then);
        let or = builder.insert(or);

        builder.insert(Self {
            predicate,
            then,
            or,
        })
    }

    pub fn predicate<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.predicate.get(tree)
    }

    pub fn then<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.then.get(tree)
    }

    pub fn or<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.or.get(tree)
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

impl Branch {
    pub fn pat<'a>(&self, tree: &'a impl NodeContainer) -> &'a Pat {
        self.pat.get(tree)
    }

    pub fn matches<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.matches.get(tree)
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Case {
    pub source: NodeId<Ident>,
    pub branches: Vec<NodeId<Branch>>,
}

impl Case {
    pub fn source<'a>(&self, tree: &'a impl NodeContainer) -> &'a Ident {
        self.source.get(tree)
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
