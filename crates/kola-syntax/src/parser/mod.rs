pub mod primitives;
pub mod rules;

use std::fmt::Debug;

pub use ext::KolaCombinator;
pub use input::ParseInput;
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
    pub recovered: Report,
}

pub fn parse<'t>(input: ParseInput<'t>, report: &mut Report) -> ParseOutput {
    let parser = rules::ModuleCombinator::COMBINATOR;

    let mut input = input;
    let report_cp = report.checkpoint();

    let result = parser.parse(&mut input, report);

    let State {
        tokens,
        builder,
        spans,
        recovered,
        ..
    } = input.state;

    match result {
        Ok(root) => {
            let tree = Some(builder.finish(root));
            ParseOutput {
                tokens,
                tree,
                spans,
                recovered,
            }
        }
        Err(e) => {
            let diag = e.extract(report, report_cp);
            report.add_diagnostic(diag);
            ParseOutput {
                tokens,
                tree: None,
                spans,
                recovered,
            }
        }
    }
}

pub struct ParseResult<T> {
    pub node: T,
    pub tokens: SemanticTokens,
    pub builder: TreeBuilder,
    pub spans: Locations,
    pub recovered: Report,
}

pub fn try_parse_with<'t, T, P>(
    input: ParseInput<'t>,
    parser: P,
) -> Result<ParseResult<T>, Diagnostic>
where
    T: Debug,
    P: Parser<ParseInput<'t>, T>,
{
    let mut input = input;
    let mut report = Report::new();
    let report_cp = report.checkpoint();

    let node = parser
        .parse(&mut input, &mut report)
        .map_err(|e| e.extract(&mut report, report_cp))?;

    let State {
        tokens,
        builder,
        spans,
        recovered,
        ..
    } = input.state;

    Ok(ParseResult {
        node,
        tokens,
        builder,
        spans,
        recovered,
    })
}
