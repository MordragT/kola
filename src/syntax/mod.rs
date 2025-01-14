use chumsky::{input::Input, span::SimpleSpan, Parser};
use error::{SyntaxErrors, SyntaxResult};

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

pub fn try_tokenize(src: &Source) -> SyntaxResult<Vec<Spanned<token::Token<'_>>>> {
    let input = src.as_str();
    let lexer = lexer::lexer();
    lexer
        .parse(input)
        .into_result()
        .map_err(|errs| SyntaxErrors::from_rich(src, errs))
}

pub fn try_parse(src: &Source, tokens: Vec<Spanned<token::Token<'_>>>) -> SyntaxResult<ast::Expr> {
    let input = tokens.as_slice().spanned((src.len()..src.len()).into());
    let parser = parser::expr_parser();
    parser
        .parse(input)
        .into_result()
        .map_err(|errs| SyntaxErrors::from_rich(src, errs))
}
