pub mod primitives;
pub mod rules;

pub use ext::ParserExt;
pub use input::ParseInput;
pub use state::{Error, Extra, State};

mod ext;
mod input;
mod state;

use chumsky::prelude::*;

use kola_span::{Diagnostic, Report};
use kola_tree::prelude::*;

use crate::{loc::Locations, token::SemanticTokens};

pub struct ParseOutput {
    pub tokens: SemanticTokens,
    pub tree: Option<Tree>,
    pub spans: Locations,
}

pub fn parse(input: ParseInput<'_>, report: &mut Report) -> ParseOutput {
    let parser = rules::module_parser();

    let mut state = State::new();

    let (root, errors) = parser
        .parse_with_state(input, &mut state)
        .into_output_errors();

    report.extend_diagnostics(errors.into_iter().map(Diagnostic::from));

    let State {
        tokens,
        builder,
        spans,
    } = state;
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

pub fn try_parse_with<'t, T>(
    input: ParseInput<'t>,
    parser: impl Parser<'t, ParseInput<'t>, T, Extra<'t>>,
) -> Result<ParseResult<T>, Vec<Error<'t>>> {
    let mut state = State::new();

    let node = parser.parse_with_state(input, &mut state).into_result()?;

    let State {
        tokens,
        builder,
        spans,
    } = state;

    Ok(ParseResult {
        node,
        tokens,
        builder,
        spans,
    })
}
