use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::Symbol;
use crate::print::TreePrinter;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Bool(bool),
    Num(f64),
    Char(char),
    Str(Symbol),
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
