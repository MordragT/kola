use std::fmt;

use crate::{Loc, SourceId, Span};

pub trait Input {
    type Token: Clone + fmt::Debug + PartialEq;

    type State;

    /// Get the source ID of the input.
    fn source_id(&self) -> SourceId;

    /// Get the current state of the parser.
    fn state(&mut self) -> &mut Self::State;

    /// The token at the current position, without advancing.
    fn peek(&self) -> Option<Self::Token>;

    /// Consume and return the current token.
    fn advance(&mut self) -> Option<Self::Token>;

    /// Check if the input has been fully consumed.
    fn is_empty(&self) -> bool;

    /// Get the span of the current token.
    fn span(&self) -> Span;

    /// Get the span of the previous token.
    fn prev_span(&self) -> Span;

    /// Save position. Does NOT save State — state mutations in failed branches
    /// are permanent
    fn checkpoint(&self) -> usize;

    /// Reset to a previously saved position. Does NOT reset State.
    fn reset(&mut self, checkpoint: usize);

    /// Get the location of the current token.
    #[inline]
    fn loc(&self) -> Loc {
        Loc::new(self.source_id(), self.span())
    }

    /// Get the location of the previous token.
    #[inline]
    fn prev_loc(&self) -> Loc {
        Loc::new(self.source_id(), self.prev_span())
    }
}

#[derive(Debug, Clone)]
pub struct SimpleInput<T, S> {
    pub source_id: SourceId,
    pub tokens: Vec<(T, Span)>,
    pub cursor: usize,
    pub state: S,
}

impl<T, S> Input for SimpleInput<T, S>
where
    T: Clone + fmt::Debug + PartialEq,
{
    type Token = T;
    type State = S;

    #[inline]
    fn source_id(&self) -> SourceId {
        self.source_id
    }

    #[inline]
    fn state(&mut self) -> &mut Self::State {
        &mut self.state
    }

    #[inline]
    fn peek(&self) -> Option<Self::Token> {
        self.tokens.get(self.cursor).map(|(t, _)| t.clone())
    }

    #[inline]
    fn advance(&mut self) -> Option<Self::Token> {
        let token = self.peek()?;
        self.cursor += 1;
        Some(token)
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.cursor >= self.tokens.len()
    }

    #[inline]
    fn span(&self) -> Span {
        self.tokens
            .get(self.cursor)
            .map(|(_, span)| *span)
            .unwrap_or_else(|| Span::new(0, 0))
    }

    #[inline]
    fn prev_span(&self) -> Span {
        if self.cursor == 0 {
            return Span::new(0, 0);
        }
        self.tokens
            .get(self.cursor - 1)
            .map(|(_, span)| *span)
            .unwrap_or_else(|| Span::new(0, 0))
    }

    #[inline]
    fn checkpoint(&self) -> usize {
        self.cursor
    }

    #[inline]
    fn reset(&mut self, checkpoint: usize) {
        self.cursor = checkpoint;
    }
}
