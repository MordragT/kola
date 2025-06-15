use std::{
    fmt,
    ops::{Add, BitAnd, BitOr, Range, Sub},
};

pub type Spanned<T> = (T, Span);

/// The most basic implementor of `Span` - akin to `Range`,
/// but `Copy` since it's not also an iterator.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    /// The start offset of the span.
    pub start: usize,
    /// The end (exclusive) offset of the span.
    pub end: usize,
}

impl Span {
    /// Creates a new `Span` with the given start and end offsets.
    #[inline]
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// Returns the length of the span
    #[inline]
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    /// Returns true if the span is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }

    /// Returns true if this span completely contains the other span
    #[inline]
    pub fn contains(&self, other: &Span) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    /// Returns true if this span contains the given position
    #[inline]
    pub fn contains_pos(&self, pos: usize) -> bool {
        pos >= self.start && pos < self.end
    }

    /// Returns the union of two spans (smallest span that contains both)
    #[inline]
    pub fn union(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// Returns the intersection of two spans, or None if they don't overlap
    #[inline]
    pub fn intersection(&self, other: &Span) -> Option<Span> {
        let start = self.start.max(other.start);
        let end = self.end.min(other.end);
        if start < end {
            Some(Span { start, end })
        } else {
            None
        }
    }

    /// Returns a span from the start of self up to (but not including) the start of other
    /// Useful for "everything before" operations
    #[inline]
    pub fn before(&self, other: &Span) -> Option<Span> {
        if self.start < other.start && other.start <= self.end {
            Some(Span {
                start: self.start,
                end: other.start.min(self.end),
            })
        } else {
            None
        }
    }

    /// Returns a span from the end of other to the end of self
    /// Useful for "everything after" operations
    #[inline]
    pub fn after(&self, other: &Span) -> Option<Span> {
        if other.end >= self.start && other.end < self.end {
            Some(Span {
                start: other.end.max(self.start),
                end: self.end,
            })
        } else {
            None
        }
    }

    /// Creates a span that covers all the given spans
    #[inline]
    pub fn covering<I>(spans: I) -> Option<Span>
    where
        I: IntoIterator<Item = Span>,
    {
        spans.into_iter().reduce(|acc, span| acc.union(&span))
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

impl BitOr<Span> for Span {
    type Output = Span;
    fn bitor(self, other: Span) -> Self::Output {
        self.union(&other)
    }
}

impl BitAnd<Span> for Span {
    type Output = Option<Span>;
    fn bitand(self, other: Span) -> Self::Output {
        self.intersection(&other)
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
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span::new(value.start, value.end)
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        let Span { start, end } = value;
        start..end
    }
}
