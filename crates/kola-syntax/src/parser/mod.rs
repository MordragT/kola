pub mod primitives;
pub mod rules;

pub use ext::KolaCombinator;
pub use input::{ParseInput, make_input};
pub use state::State;

pub mod ext;
mod input;
pub mod state;

use kola_span::parser::Parser;
use kola_span::{Diagnostic, Report, primitive::Lazy};
use kola_tree::prelude::*;

use crate::{loc::Locations, token::SemanticTokens};

pub struct ParseOutput {
    pub tokens: SemanticTokens,
    pub tree: Option<Tree>,
    pub spans: Locations,
}

pub fn parse<'t>(input: ParseInput<'t>, report: &mut Report) -> ParseOutput {
    let parser = rules::ModuleCombinator::COMBINATOR;

    let mut input = input;

    let result = parser.parse(&mut input, report);

    let State {
        tokens,
        builder,
        spans,
        ..
    } = input.state;

    match result {
        Ok(root) => {
            let tree = Some(builder.finish(root));
            ParseOutput {
                tokens,
                tree,
                spans,
            }
        }
        Err(diag) => {
            report.add_diagnostic(diag);
            ParseOutput {
                tokens,
                tree: None,
                spans,
            }
        }
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
) -> Result<ParseResult<T>, Diagnostic>
where
    P: Parser<ParseInput<'t>, T>,
{
    let mut input = input;
    let mut report = Report::new();

    let node = parser.parse(&mut input, &mut report)?;

    let State {
        tokens,
        builder,
        spans,
        ..
    } = input.state;

    Ok(ParseResult {
        node,
        tokens,
        builder,
        spans,
    })
}
