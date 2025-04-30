use kola_tree::meta::UniformPhase;
use span::Span;

pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

pub mod prelude {
    pub use crate::SyntaxPhase;
    pub use crate::lexer::{TokenizeResult, tokenize};
    pub use crate::parser::{ParseResult, parse};
    pub use crate::span::*;
    pub use crate::token::{
        SemanticToken, SemanticTokenKind, Token, TokenPrinter, TokenSlice, Tokens,
    };
}

#[derive(Clone, Copy, Debug)]
pub struct SyntaxPhase;

impl UniformPhase for SyntaxPhase {
    type Meta = Span;
}
