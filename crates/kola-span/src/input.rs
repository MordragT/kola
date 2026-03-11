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
    fn loc(&self) -> Loc {
        Loc::new(self.source_id(), self.span())
    }

    /// Get the location of the previous token.
    fn prev_loc(&self) -> Loc {
        Loc::new(self.source_id(), self.prev_span())
    }
}
