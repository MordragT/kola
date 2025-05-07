use miette::SourceSpan;
use std::{fmt, ops::Range};

use kola_print::prelude::*;
use kola_tree::prelude::*;

use crate::SyntaxPhase;

/// The most basic implementor of `Span` - akin to `Range`,
/// but `Copy` since it's not also an iterator.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    /// The start offset of the span.
    pub start: usize,
    /// The end (exclusive) offset of the span.
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn into_range(self) -> Range<usize> {
        Range {
            start: self.start,
            end: self.end,
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl IntoIterator for Span {
    type IntoIter = Range<usize>;
    type Item = usize;

    fn into_iter(self) -> Self::IntoIter {
        self.start..self.end
    }
}

impl chumsky::span::Span for Span {
    type Context = ();
    type Offset = usize;

    fn new(_context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        ()
    }

    fn start(&self) -> Self::Offset {
        self.start.clone()
    }

    fn end(&self) -> Self::Offset {
        self.end.clone()
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        let Span { start, end } = value;
        SourceSpan::new(start.into(), end)
    }
}

pub type Spanned<T> = (T, Span);
pub type SpanInfo = Metadata<SyntaxPhase>;

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
