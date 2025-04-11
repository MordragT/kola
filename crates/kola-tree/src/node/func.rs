use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Ident};
use crate::{id::NodeId, print::TreePrinter, tree::NodeContainer};

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
