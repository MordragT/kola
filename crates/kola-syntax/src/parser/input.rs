use kola_span::{SourceId, Span, input::Input};
use kola_utils::interner::StrInterner;

use super::State;
use crate::token::{Token, Tokens};

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
}

impl<'t> Input for ParseInput<'t> {
    type Token = Token<'t>;
    type State = State<'t>;

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
