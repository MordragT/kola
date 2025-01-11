use chumsky::{input::Input, span::SimpleSpan, Parser};
use error::{SyntaxErrors, SyntaxResult};
use miette::NamedSource;
use std::sync::Arc;

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod token;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Source(NamedSource<Arc<str>>);

impl Source {
    pub fn new(name: impl AsRef<str>, source: impl AsRef<str>) -> Self {
        Self(NamedSource::new(name, Arc::from(source.as_ref())))
    }

    pub fn name(&self) -> &str {
        self.0.name()
    }

    pub fn as_str(&self) -> &str {
        self.0.inner()
    }

    pub fn named_source(&self) -> NamedSource<Arc<str>> {
        self.0.clone()
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }
}

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
