use std::{fmt, ops::Range};

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
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
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
