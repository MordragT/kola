use chumsky::{extra::SimpleState, input::Input, span::SimpleSpan, Parser};
use error::{SyntaxError, SyntaxErrors};
use token::Tokens;
use tree::SyntaxTree;

use crate::source::Source;

pub mod error;
pub mod lexer;
pub mod parser;
pub mod print;
pub mod token;
pub mod tree;
// pub mod visit;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

pub struct TokenizeResult<'a> {
    pub tokens: Option<Tokens<'a>>,
    pub errors: SyntaxErrors,
}

pub fn tokenize(input: &str) -> TokenizeResult<'_> {
    let lexer = lexer::lexer();
    let (tokens, errors) = lexer.parse(input).into_output_errors();

    let errors = errors
        .into_iter()
        .map(SyntaxError::from)
        .collect::<SyntaxErrors>();
    TokenizeResult { tokens, errors }
}

pub struct ParseResult {
    pub tree: Option<SyntaxTree>,
    pub errors: SyntaxErrors,
}

pub fn parse(tokens: Tokens<'_>, eoi: Span) -> ParseResult {
    let input = tokens.as_slice().map(eoi, |(t, s)| (t, s));
    let parser = parser::expr_parser();

    let mut state = SimpleState(SyntaxTree::new());

    let (root, errors) = parser
        .parse_with_state(input, &mut state)
        .into_output_errors();

    let errors = errors
        .into_iter()
        .map(SyntaxError::from)
        .collect::<SyntaxErrors>();

    let tree = root.and(Some(state.0));

    ParseResult { tree, errors }
}
