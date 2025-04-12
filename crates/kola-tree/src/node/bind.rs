use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Name};
use crate::{
    id::NodeId,
    print::TreePrinter,
    tree::{NodeContainer, TreeBuilder},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Let {
    pub name: NodeId<Name>,
    pub value: NodeId<Expr>,
    pub inside: NodeId<Expr>,
}

impl Let {
    pub fn new_in(
        name: Name,
        value: Expr,
        inside: Expr,
        builder: &mut TreeBuilder,
    ) -> NodeId<Self> {
        let name = builder.insert(name);
        let value = builder.insert(value);
        let inside = builder.insert(inside);

        builder.insert(Self {
            name,
            value,
            inside,
        })
    }

    pub fn name<'a>(&self, tree: &'a impl NodeContainer) -> &'a Name {
        self.name.get(tree)
    }

    pub fn value<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.value.get(tree)
    }

    pub fn inside<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.inside.get(tree)
    }
}

impl Printable<TreePrinter> for Let {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            name,
            value,
            inside,
        } = self;

        let head = "Let".blue().display_in(arena);

        let name = name.notate(with, arena);
        let value = value.notate(with, arena);
        let inside = inside.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
            arena.notate(", inside = "),
            inside.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("value = "),
            value,
            arena.newline(),
            arena.notate("inside = "),
            inside,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}
