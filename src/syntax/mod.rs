use chumsky::{input::Input, span::SimpleSpan, Parser};
use error::{SyntaxError, SyntaxErrors};

use crate::source::Source;

pub mod ast;
pub mod error;
pub mod lexer;
pub mod node;
pub mod parser;
pub mod token;
pub mod visit;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

pub type Tokens<'a> = Vec<Spanned<token::Token<'a>>>;

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
    pub ast: Option<ast::Expr>,
    pub errors: SyntaxErrors,
}

pub fn parse(tokens: Tokens<'_>, eoi: Span) -> ParseResult {
    let input = tokens.as_slice().spanned(eoi);
    let parser = parser::expr_parser();
    let (ast, errors) = parser.parse(input).into_output_errors();

    let errors = errors
        .into_iter()
        .map(SyntaxError::from)
        .collect::<SyntaxErrors>();
    ParseResult { ast, errors }
}
