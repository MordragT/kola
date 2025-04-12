use derive_more::From;
use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::Symbol;
use crate::print::TreePrinter;

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[from(forward)]
pub struct Name(pub Symbol);

impl Name {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl PartialEq<Symbol> for Name {
    fn eq(&self, other: &Symbol) -> bool {
        &self.0 == other
    }
}

impl PartialEq<str> for Name {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
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
