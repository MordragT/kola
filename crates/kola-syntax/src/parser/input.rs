use std::ops::Range;

use chumsky::{input::ValueInput, prelude::*};
use kola_span::{Loc, SourceId};

use crate::token::{Token, Tokens};

pub struct ParseInput<'t> {
    pub source: SourceId,
    pub tokens: Tokens<'t>,
}

impl<'t> ParseInput<'t> {
    pub fn new(source: SourceId, tokens: Tokens<'t>) -> Self {
        Self { source, tokens }
    }
}

impl<'t> Input<'t> for ParseInput<'t> {
    type Span = Loc;
    type Token = Token<'t>;
    type MaybeToken = Token<'t>;
    type Cursor = usize;
    type Cache = (Tokens<'t>, SourceId);

    fn begin(self) -> (Self::Cursor, Self::Cache) {
        let Self { source, tokens } = self;

        (0, (tokens, source))
    }

    fn cursor_location(cursor: &Self::Cursor) -> usize {
        *cursor
    }

    unsafe fn next_maybe(
        cache: &mut Self::Cache,
        cursor: &mut Self::Cursor,
    ) -> Option<Self::MaybeToken> {
        if let Some(tok) = cache.0.get(*cursor) {
            *cursor += 1;
            Some(tok.0)
        } else {
            None
        }
    }

    unsafe fn span(cache: &mut Self::Cache, range: Range<&Self::Cursor>) -> Self::Span {
        Loc::from_range(
            cache.1,
            // TODO is this correct?
            cache.0[*range.start].1.span.start..cache.0[*range.end - 1].1.span.end,
        )
    }
}

impl<'t> ValueInput<'t> for ParseInput<'t> {
    unsafe fn next(cache: &mut Self::Cache, cursor: &mut Self::Cursor) -> Option<Self::Token> {
        unsafe { Self::next_maybe(cache, cursor) }
    }
}
