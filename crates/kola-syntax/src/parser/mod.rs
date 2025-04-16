pub use ext::ParserExt;
pub use state::{Extra, State, StateRepr};

pub mod primitives;
pub mod rules;

mod ext;
mod state;

use chumsky::{input::ValueInput, prelude::*};
use kola_tree::prelude::*;

use crate::{
    SyntaxPhase,
    error::{SyntaxError, SyntaxErrors},
    span::{Span, SpanMetadata},
    token::{Token, Tokens},
};

pub struct ParseResult {
    pub tree: Option<Tree>,
    pub spans: SpanMetadata,
    pub errors: SyntaxErrors,
}

pub fn parse(tokens: Tokens<'_>, eoi: Span) -> ParseResult {
    let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));
    let parser = rules::expr_parser();

    let mut state = State::from(StateRepr::new());

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

type Output<T> = (T, TreeBuilder, MetaVec<SyntaxPhase>);

pub fn try_parse_with<'src, T, P, I>(input: I, parser: P) -> Result<Output<T>, SyntaxErrors>
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
    P: Parser<'src, I, T, Extra<'src>>,
{
    let mut state = State::from(StateRepr::new());

    let node = parser
        .parse_with_state(input, &mut state)
        .into_result()
        .map_err(|errs| {
            errs.into_iter()
                .map(SyntaxError::from)
                .collect::<SyntaxErrors>()
        })?;

    let StateRepr { builder, meta } = state.0;
    Ok((node, builder, meta))
}
