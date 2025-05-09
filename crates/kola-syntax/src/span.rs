use kola_print::prelude::*;
use kola_tree::prelude::*;

#[derive(Clone, Copy, Debug)]
pub struct SpanPhase;

impl UniformPhase for SpanPhase {
    type Meta = kola_span::Span;
}

pub type SpanInfo = Metadata<SpanPhase>;

#[derive(Debug, Clone)]
pub struct SpanDecorator(pub SpanInfo);

impl Decorator for SpanDecorator {
    fn decorate<'a>(
        &'a self,
        notation: Notation<'a>,
        with: Id<()>,
        arena: &'a Bump,
    ) -> Notation<'a> {
        let span = self.0.get(with).inner_ref();
        let head = span.display_in(arena);

        let single = arena.just(' ').then(notation.clone().flatten(arena), arena);
        let multi = arena.newline().then(notation, arena);

        head.then(single.or(multi, arena), arena)
    }
}
