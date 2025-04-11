use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::Symbol;
use crate::print::TreePrinter;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Ident(pub Symbol);

impl Ident {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

// impl Deref for Ident {
//     type Target = Symbol;

//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }

impl PartialEq<Symbol> for Ident {
    fn eq(&self, other: &Symbol) -> bool {
        &self.0 == other
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, other: &str) -> bool {
        self.0.as_str() == other
    }
}

impl From<Symbol> for Ident {
    fn from(value: Symbol) -> Self {
        Self(value)
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(value: &'a str) -> Self {
        Self(Symbol::from(value))
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Self(Symbol::from(value))
    }
}

impl Printable<TreePrinter> for Ident {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "Ident".cyan().display_in(arena);

        let ident = self
            .0
            .yellow()
            .display_in(arena)
            .enclose_by(arena.just('"'), arena);

        let single = [arena.just(' '), ident.clone().flatten(arena)].concat_in(arena);
        let multi = [arena.newline(), ident].concat_in(arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}
