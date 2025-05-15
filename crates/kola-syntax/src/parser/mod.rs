pub mod primitives;
pub mod rules;

pub use ext::KolaParser;
pub use input::ParseInput;
use kola_utils::interner::HasMutStrInterner;
pub use state::{Error, Extra, State};

mod ext;
mod input;
mod state;

use chumsky::prelude::*;

use kola_span::{Diagnostic, HasMutReport};
use kola_tree::prelude::*;

use crate::{loc::Locations, token::SemanticTokens};

pub struct ParseOutput {
    pub tokens: SemanticTokens,
    pub tree: Option<Tree>,
    pub spans: Locations,
}

pub fn parse<C>(input: ParseInput<'_>, ctx: &mut C) -> ParseOutput
where
    C: HasMutReport + HasMutStrInterner,
{
    let parser = rules::module_parser();

    let mut state = State::new(ctx);

    let (root, errors) = parser
        .parse_with_state(input, &mut state)
        .into_output_errors();

    let State {
        tokens,
        builder,
        spans,
        ctx,
    } = state;

    ctx.report_mut()
        .extend_diagnostics(errors.into_iter().map(Diagnostic::from));

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

pub fn try_parse_with<'t, T, C, P>(
    input: ParseInput<'t>,
    parser: P,
    ctx: &'t mut C,
) -> Result<ParseResult<T>, Vec<Error<'t>>>
where
    P: KolaParser<'t, T, C>,
    C: HasMutStrInterner + 't,
{
    let mut state = State::new(ctx);

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
