pub mod primitives;
pub mod rules;

pub use ext::KolaParser;
pub use input::ParseInput;
pub use state::{Error, Extra, State};

mod ext;
mod input;
mod state;

use chumsky::prelude::*;

use kola_span::{Diagnostic, Report};
use kola_tree::prelude::*;
use kola_utils::interner::StrInterner;

use crate::{loc::Locations, token::SemanticTokens};

pub struct ParseOutput {
    pub tokens: SemanticTokens,
    pub tree: Option<Tree>,
    pub spans: Locations,
}

pub fn parse<'t>(
    input: ParseInput<'t>,
    interner: &'t mut StrInterner,
    report: &'t mut Report,
) -> ParseOutput {
    let parser = rules::root_parser();

    let mut state = State::new(interner);

    let (root, errors) = parser
        .parse_with_state(input, &mut state)
        .into_output_errors();

    let State {
        tokens,
        builder,
        spans,
        ..
    } = state;

    report.extend_diagnostics(errors.into_iter().map(Diagnostic::from));

    let tree = root.map(|root| builder.finish(root));

    ParseOutput {
        tokens,
        tree,
        spans,
    }
}

pub struct ParseResult<T> {
    pub node: T,
    pub tokens: SemanticTokens,
    pub builder: TreeBuilder,
    pub spans: Locations,
}

pub fn try_parse_with<'t, T, P>(
    input: ParseInput<'t>,
    parser: P,
    interner: &'t mut StrInterner,
) -> Result<ParseResult<T>, Vec<Error<'t>>>
where
    P: KolaParser<'t, T>,
{
    let mut state = State::new(interner);

    let node = parser.parse_with_state(input, &mut state).into_result()?;

    let State {
        tokens,
        builder,
        spans,
        ..
    } = state;

    Ok(ParseResult {
        node,
        tokens,
        builder,
        spans,
    })
}
