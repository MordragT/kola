use kola_span::{SourceId, Span, input::Input};
use kola_utils::interner::StrInterner;

use super::{State, state::StateCheckpoint};
use crate::token::{Token, Tokens};

#[derive(Debug, Clone, Copy)]
pub struct ParseCheckpoint {
    pub cursor: usize,
    pub state: StateCheckpoint,
}

#[derive(Debug)]
pub struct ParseInput<'t> {
    pub source_id: SourceId,
    pub tokens: Vec<(Token<'t>, Span)>,
    pub cursor: usize,
    pub state: State<'t>,
}

impl<'t> ParseInput<'t> {
    pub fn new(source_id: SourceId, tokens: Tokens<'t>, interner: &'t mut StrInterner) -> Self {
        // TODO: Once doc comments are integrated into the AST, stop filtering
        // CommentT::Doc here and instead handle them in the parser rules so
        // they can be attached to declarations (let, type, module, etc.).
        let tokens: Vec<(Token<'t>, Span)> = tokens
            .into_iter()
            .filter(|(tok, _)| !matches!(tok, Token::Comment(_)))
            .map(|(tok, loc)| (tok, loc.span))
            .collect();

        Self {
            source_id,
            tokens,
            cursor: 0,
            state: State::new(interner),
        }
    }

    // TODO migrate away from this by just putting the elements of state directly in here
    pub fn state(&mut self) -> &mut State<'t> {
        &mut self.state
    }
}

impl<'t> Input for ParseInput<'t> {
    type Token = Token<'t>;
    type Checkpoint = ParseCheckpoint;

    #[inline]
    fn source_id(&self) -> SourceId {
        self.source_id
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
    fn checkpoint(&self) -> ParseCheckpoint {
        ParseCheckpoint {
            cursor: self.cursor,
            state: self.state.checkpoint(),
        }
    }

    #[inline]
    fn reset(&mut self, checkpoint: ParseCheckpoint) {
        self.cursor = checkpoint.cursor;
        self.state.reset(checkpoint.state);
    }
}
