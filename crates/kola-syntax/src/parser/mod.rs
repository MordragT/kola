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
    error::{SourceDiagnostic, SourceResult},
    span::{Span, SpanMetadata},
    token::{Token, Tokens},
};

pub struct ParseResult {
    pub tree: Option<Tree>,
    pub spans: SpanMetadata,
    pub errors: Vec<SourceDiagnostic>,
}

pub fn parse(tokens: Tokens<'_>, eoi: Span) -> ParseResult {
    let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));
    let parser = rules::module_parser();

    let mut state = State::from(StateRepr::new());

    let (root, errors) = parser
        .parse_with_state(input, &mut state)
        .into_output_errors();

    let errors = errors.into_iter().map(SourceDiagnostic::from).collect();

    let StateRepr { builder, meta } = state.0;
    let tree = root.map(|root| builder.finish(root));

    ParseResult {
        tree,
        spans: meta.into_metadata(),
        errors,
    }
}

type Output<T> = (T, TreeBuilder, MetaVec<SyntaxPhase>);

pub fn try_parse_with<'t, T, P, I>(input: I, parser: P) -> SourceResult<Output<T>>
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
    P: Parser<'t, I, T, Extra<'t>>,
{
    let mut state = State::from(StateRepr::new());

    let node = parser
        .parse_with_state(input, &mut state)
        .into_result()
        .map_err(|errs| {
            errs.into_iter()
                .map(SourceDiagnostic::from)
                .collect::<Vec<_>>()
        })?;

    let StateRepr { builder, meta } = state.0;
    Ok((node, builder, meta))
}
