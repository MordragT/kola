use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::Symbol;
use crate::print::TreePrinter;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Name(pub Symbol);

impl Name {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

// impl Deref for Name {
//     type Target = Symbol;

//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }

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

impl From<Symbol> for Name {
    fn from(value: Symbol) -> Self {
        Self(value)
    }
}

impl<'a> From<&'a str> for Name {
    fn from(value: &'a str) -> Self {
        Self(Symbol::from(value))
    }
}

impl From<String> for Name {
    fn from(value: String) -> Self {
        Self(Symbol::from(value))
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
