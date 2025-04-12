use derive_more::From;
use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::Expr;
use crate::{id::NodeId, print::TreePrinter};

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct List(pub Vec<NodeId<Expr>>);

impl Printable<TreePrinter> for List {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let kind = "List".blue().display_in(arena);

        let values = self.0.gather(with, arena);

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
