use kola_tree::UniformPhase;
use span::Span;

pub mod error;
pub mod lexer;
pub mod parser;
pub mod source;
pub mod span;
pub mod token;

pub mod prelude {
    pub use crate::SyntaxPhase;
    pub use crate::error::{SyntaxError, SyntaxErrors, SyntaxReport};
    pub use crate::lexer::{TokenizeResult, tokenize};
    pub use crate::parser::{ParseResult, parse};
    pub use crate::source::Source;
    pub use crate::span::*;
    pub use crate::token::{Token, TokenKind, TokenPrinter, Tokens};
}

#[derive(Clone, Copy, Debug)]
pub struct SyntaxPhase;

impl UniformPhase for SyntaxPhase {
    type Meta = Span;
}
