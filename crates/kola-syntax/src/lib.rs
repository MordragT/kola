pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

pub mod prelude {
    pub use crate::lexer::{TokenizeResult, tokenize};
    pub use crate::parser::{ParseResult, parse};
    pub use crate::span::{SpanDecorator, SpanInfo, SpanPhase};
    pub use crate::token::{
        SemanticToken, SemanticTokens, Token, TokenPrinter, TokenSlice, Tokens,
    };
}
