use std::ops::Range;

use chumsky::{input::ValueInput, prelude::*};
use kola_span::{Loc, SourceId, Span};

use crate::token::{Token, Tokens};

pub struct ParseInput<'t> {
    pub source: SourceId,
    pub tokens: Tokens<'t>,
    pub eoi: Span,
}

impl<'t> ParseInput<'t> {
    pub fn new(source: SourceId, tokens: Tokens<'t>, len: usize) -> Self {
        Self {
            source,
            tokens,
            eoi: Span::new(len, len),
        }
    }
}

impl<'t> Input<'t> for ParseInput<'t> {
    type Span = Loc;
    type Token = Token<'t>;
    type MaybeToken = Token<'t>;
    type Cursor = usize;
    type Cache = ParseInput<'t>;

    fn begin(self) -> (Self::Cursor, Self::Cache) {
        (0, self)
    }

    fn cursor_location(cursor: &Self::Cursor) -> usize {
        *cursor
    }

    unsafe fn next_maybe(
        cache: &mut Self::Cache,
        cursor: &mut Self::Cursor,
    ) -> Option<Self::MaybeToken> {
        if let Some(tok) = cache.tokens.get(*cursor) {
            *cursor += 1;
            Some(tok.0)
        } else {
            None
        }
    }

    unsafe fn span(cache: &mut Self::Cache, range: Range<&Self::Cursor>) -> Self::Span {
        if let Some((_token, loc)) = cache.tokens.get(*range.start).copied() {
            loc
        } else {
            Loc::new(cache.source, cache.eoi)
        }
    }
}

impl<'t> ValueInput<'t> for ParseInput<'t> {
    unsafe fn next(cache: &mut Self::Cache, cursor: &mut Self::Cursor) -> Option<Self::Token> {
        unsafe { Self::next_maybe(cache, cursor) }
    }
}
