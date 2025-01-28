use chumsky::span::SimpleSpan;
use kola_print::prelude::*;
use kola_tree::prelude::*;

use crate::SyntaxPhase;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);
pub type SpanMetadata = Metadata<SyntaxPhase>;

pub struct SpanDecorator(pub SpanMetadata);

impl Decorator for SpanDecorator {
    fn decorate<'a>(
        &'a self,
        notation: Notation<'a>,
        with: NodeId<()>,
        arena: &'a Bump,
    ) -> Notation<'a> {
        let span = self.0.get(with).inner_ref();
        let head = span.display_in(arena);

        let single = arena.just(' ').then(notation.clone().flatten(arena), arena);
        let multi = arena.newline().then(notation, arena);

        head.then(single.or(multi, arena), arena)
    }
}
