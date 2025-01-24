use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Metadata, Node, NodeId, Tree};
use crate::syntax::print::prelude::*;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct List {
    pub values: Vec<NodeId<Expr>>,
}

impl<M> Printable<Tree<M>> for List
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        let kind = "List".blue().display_in(arena);

        let values = self.values.gather(with, arena);

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

impl TryFrom<Node> for List {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::List(l) => Ok(l),
            _ => Err(()),
        }
    }
}
