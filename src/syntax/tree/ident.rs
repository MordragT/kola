use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Node, Symbol, Tree};
use crate::syntax::print::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Ident(pub Symbol);

impl<M> Printable<Tree<M>> for Ident {
    fn notate<'a>(&'a self, _with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
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

impl TryFrom<Node> for Ident {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Ident(i) => Ok(i),
            _ => Err(()),
        }
    }
}
