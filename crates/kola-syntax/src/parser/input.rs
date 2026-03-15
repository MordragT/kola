use kola_span::{SourceId, Span, input::SimpleInput};

use super::State;
use crate::token::{Token, Tokens};

pub type ParseInput<'t> = SimpleInput<Token<'t>, State<'t>>;

pub fn make_input<'t>(
    source: SourceId,
    tokens: Tokens<'t>,
    len: usize,
    state: State<'t>,
) -> ParseInput<'t> {
    // TODO: Once doc comments are integrated into the AST, stop filtering
    // CommentT::Doc here and instead handle them in the parser rules so
    // they can be attached to declarations (let, type, module, etc.).
    let tokens: Vec<(Token<'t>, Span)> = tokens
        .into_iter()
        .filter(|(tok, _)| !matches!(tok, Token::Comment(_)))
        .map(|(tok, loc)| (tok, loc.span))
        .collect();

    SimpleInput {
        source_id: source,
        tokens,
        cursor: 0,
        state,
    }
}
