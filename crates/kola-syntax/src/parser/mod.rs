pub use ext::ParserExt;
pub use state::{Error, Extra, State, StateRepr};

pub mod primitives;
pub mod rules;

mod ext;
mod state;

use chumsky::{input::ValueInput, prelude::*};

use kola_span::Span;
use kola_tree::prelude::*;
use kola_utils::StrInterner;

use crate::{
    span::{SpanInfo, SpanPhase},
    token::{SemanticTokens, Token, TokenSlice},
};

pub struct ParseResult<'t> {
    pub tokens: SemanticTokens,
    pub interner: StrInterner,
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

    let StateRepr {
        tokens,
        builder,
        meta,
        interner,
    } = state.0;
    let tree = root.map(|root| builder.finish(root));

    ParseResult {
        tokens,
        interner,
        tree,
        spans: meta.into_metadata(),
        errors,
    }
}

pub struct ParseOutput<T> {
    pub node: T,
    pub tokens: SemanticTokens,
    pub interner: StrInterner,
    pub builder: TreeBuilder,
    pub spans: MetaVec<SpanPhase>,
}

pub fn try_parse_with<'t, T, P, I>(input: I, parser: P) -> Result<ParseOutput<T>, Vec<Error<'t>>>
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
    P: Parser<'t, I, T, Extra<'t>>,
{
    let mut state = State::from(StateRepr::new());

    let node = parser.parse_with_state(input, &mut state).into_result()?;

    let StateRepr {
        tokens,
        builder,
        meta,
        interner,
    } = state.0;

    Ok(ParseOutput {
        node,
        tokens,
        interner,
        builder,
        spans: meta,
    })
}
