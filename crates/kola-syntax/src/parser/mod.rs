pub use ext::ParserExt;
pub use state::{Extra, State, StateRepr};

pub mod expr;
mod ext;
mod state;

use chumsky::prelude::*;
use kola_tree::{MetaContainer, Tree};

use crate::{
    Span, SpanMetadata,
    error::{SyntaxError, SyntaxErrors},
    token::Tokens,
};

pub struct ParseResult {
    pub tree: Option<Tree>,
    pub spans: SpanMetadata,
    pub errors: SyntaxErrors,
}

pub fn parse(tokens: Tokens<'_>, eoi: Span) -> ParseResult {
    let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));
    let parser = expr::expr_parser();

    let repr = StateRepr::new();
    let mut state = State::from(repr);

    let (root, errors) = parser
        .parse_with_state(input, &mut state)
        .into_output_errors();

    let errors = errors
        .into_iter()
        .map(SyntaxError::from)
        .collect::<SyntaxErrors>();

    let StateRepr { builder, meta } = state.0;
    let tree = root.map(|root| builder.finish(root));

    ParseResult {
        tree,
        spans: meta.into_metadata(),
        errors,
    }
}
