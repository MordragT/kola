use kola_utils::interner::PathKey;
use std::{fmt, ops::Range};

use crate::{Span, source::SourceId};

pub type Located<T> = (T, Loc);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loc {
    pub path: SourceId,
    pub span: Span,
}

impl fmt::Debug for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { path, span } = self;
        write!(f, "{span} in {path}")
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { path, span } = self;
        write!(f, "{span} in {path}")
    }
}

impl Loc {
    #[inline]
    pub fn new(path: SourceId, span: Span) -> Self {
        Self { path, span }
    }

    #[inline]
    pub fn from_range(path: SourceId, range: Range<usize>) -> Self {
        Self::new(path, Span::from(range))
    }

    #[inline]
    pub fn source(self) -> SourceId {
        self.path
    }

    #[inline]
    pub fn span(self) -> Span {
        self.span
    }

    /// Returns the length of the loc
    #[inline]
    pub fn len(self) -> usize {
        self.span.len()
    }

    /// Returns true if the loc is empty
    #[inline]
    pub fn is_empty(self) -> bool {
        self.span.is_empty()
    }

    /// Returns true if this loc completely contains the other loc
    #[inline]
    pub fn contains(self, other: Self) -> bool {
        debug_assert_eq!(self.path, other.path);

        self.span.contains(other.span)
    }

    /// Returns true if this loc contains the given position
    #[inline]
    pub fn contains_pos(self, pos: usize) -> bool {
        self.span.contains_pos(pos)
    }

    /// Returns the union of two locs (smallest loc that contains both)
    #[inline]
    pub fn union(self, other: Self) -> Self {
        debug_assert_eq!(self.path, other.path);

        Self::new(self.path, self.span.union(other.span))
    }

    /// Returns the intersection of two locs, or None if they don't overlap
    #[inline]
    pub fn intersection(self, other: Self) -> Option<Self> {
        debug_assert_eq!(self.path, other.path);

        self.span
            .intersection(other.span)
            .map(|span| Self::new(self.path, span))
    }

    /// Returns a loc from the start of self up to (but not including) the start of other
    /// Useful for "everything before" operations
    #[inline]
    pub fn before(self, other: Self) -> Option<Self> {
        debug_assert_eq!(self.path, other.path);

        self.span
            .before(other.span)
            .map(|span| Self::new(self.path, span))
    }

    /// Returns a loc from the end of other to the end of self
    /// Useful for "everything after" operations
    #[inline]
    pub fn after(self, other: Self) -> Option<Self> {
        debug_assert_eq!(self.path, other.path);

        self.span
            .after(other.span)
            .map(|span| Self::new(self.path, span))
    }

    /// Creates a loc that covers all the given locs
    #[inline]
    pub fn covering<I>(locs: I) -> Option<Self>
    where
        I: IntoIterator<Item = Self>,
    {
        locs.into_iter().reduce(|acc, span| {
            debug_assert_eq!(acc.path, span.path);

            acc.union(span)
        })
    }

    /// Creates a loc that covers all the locs in the given iterator of `Located<T>`
    pub fn covering_located<'a, T, I>(located_iter: I) -> Option<Loc>
    where
        I: IntoIterator<Item = &'a Located<T>>,
        T: 'a,
    {
        located_iter
            .into_iter()
            .map(|&(_, loc)| loc)
            .reduce(Loc::union)
    }
}

impl IntoIterator for Loc {
    type Item = usize;
    type IntoIter = Range<usize>;

    fn into_iter(self) -> Self::IntoIter {
        self.span.into_iter()
    }
}

impl chumsky::span::Span for Loc {
    type Context = PathKey;
    type Offset = usize;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Loc::new(context, range.into())
    }

    fn context(&self) -> Self::Context {
        self.path
    }

    fn start(&self) -> Self::Offset {
        self.span.start
    }

    fn end(&self) -> Self::Offset {
        self.span.end
    }
}

impl ariadne::Span for Loc {
    type SourceId = SourceId;

    fn source(&self) -> &Self::SourceId {
        &self.path
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

impl From<Loc> for Range<usize> {
    fn from(value: Loc) -> Self {
        value.span.into()
    }
}
