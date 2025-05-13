use kola_utils::interner::PathKey;
use std::{fmt, ops::Range};

use crate::Span;

pub type Located<T> = (T, Loc);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loc {
    pub path: PathKey,
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
    pub fn new(path: PathKey, span: Span) -> Self {
        Self { path, span }
    }

    #[inline]
    pub fn from_range(path: PathKey, range: Range<usize>) -> Self {
        Self::new(path, Span::from(range))
    }

    #[inline]
    pub fn source(&self) -> PathKey {
        self.path
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.span
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
    type SourceId = PathKey;

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
