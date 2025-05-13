use kola_print::prelude::*;
use kola_tree::prelude::*;

#[derive(Clone, Copy, Debug)]
pub struct LocPhase;

impl UniformPhase for LocPhase {
    type Meta = kola_span::Loc;
}

pub type Locations = MetaVec<LocPhase>;

#[derive(Debug, Clone)]
pub struct LocDecorator(pub Locations);

impl Decorator for LocDecorator {
    fn decorate<'a>(
        &'a self,
        notation: Notation<'a>,
        with: usize,
        arena: &'a Bump,
    ) -> Notation<'a> {
        let span = self.0.get(with).inner_ref();
        let head = span.display_in(arena);

        let single = arena.just(' ').then(notation.clone().flatten(arena), arena);
        let multi = arena.newline().then(notation, arena);

        head.then(single.or(multi, arena), arena)
    }
}
