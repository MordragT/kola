pub use ext::ParserExt;
pub use state::{Error, Extra, State, StateRepr};

pub mod primitives;
pub mod rules;

mod ext;
mod state;

use chumsky::{input::ValueInput, prelude::*};
use kola_tree::prelude::*;

use crate::{
    SyntaxPhase,
    span::{Span, SpanInfo},
    token::{Token, TokenSlice},
};

pub struct ParseResult<'t> {
    pub tree: Option<Tree>,
    pub spans: SpanInfo,
    pub errors: Vec<Error<'t>>,
}

pub fn parse<'t>(tokens: TokenSlice<'t>, eoi: Span) -> ParseResult<'t> {
    let input = tokens.map(eoi, |(t, s)| (t, s));
    let parser = rules::module_parser();

    let mut state = State::from(StateRepr::new());

    let (root, errors) = parser
        .parse_with_state(input, &mut state)
        .into_output_errors();

    // let errors = errors.into_iter().map(SourceDiagnostic::from).collect();

    let StateRepr { builder, meta } = state.0;
    let tree = root.map(|root| builder.finish(root));

    ParseResult {
        tree,
        spans: meta.into_metadata(),
        errors,
    }
}

type Output<T> = (T, TreeBuilder, MetaVec<SyntaxPhase>);

pub fn try_parse_with<'t, T, P, I>(input: I, parser: P) -> Result<Output<T>, Vec<Error<'t>>>
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
    P: Parser<'t, I, T, Extra<'t>>,
{
    let mut state = State::from(StateRepr::new());

    let node = parser.parse_with_state(input, &mut state).into_result()?;

    let StateRepr { builder, meta } = state.0;
    Ok((node, builder, meta))
}
