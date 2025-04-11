use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::Expr;
use crate::{id::NodeId, print::TreePrinter, tree::NodeContainer};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Call {
    pub func: NodeId<Expr>,
    pub arg: NodeId<Expr>,
}

impl Call {
    pub fn func<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.func.get(tree)
    }

    pub fn arg<'a>(&self, tree: &'a impl NodeContainer) -> &'a Expr {
        self.arg.get(tree)
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
