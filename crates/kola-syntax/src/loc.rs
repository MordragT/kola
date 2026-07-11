use std::collections::HashMap;

use kola_print::prelude::*;
use kola_span::{Loc, SourceId};
use kola_tree::prelude::*;

pub type LocVec = UniversalStorage<Loc>;
pub type LocMap = HashMap<SourceId, LocVec>;

#[derive(Debug, Clone)]
pub struct LocDecorator<'a>(pub &'a LocVec);

impl<'a> Decorator<'a> for LocDecorator<'a> {
    fn decorate(&self, notation: Notation<'a>, with: AnyId, arena: &'a Bump) -> Notation<'a> {
        let span = *self.0.get_any(with);
        let head = span.display_in(arena);

        let single = arena.just(' ').then(notation.clone().flatten(arena), arena);
        let multi = arena.newline().then(notation, arena);

        head.then(single.or(multi, arena), arena)
    }
}
