pub mod lexer;
pub mod loc;
pub mod parser;
pub mod token;

pub mod prelude {
    pub use crate::lexer::{TokenizeResult, tokenize};
    pub use crate::loc::{LocDecorator, LocPhase, Locations};
    pub use crate::parser::{INTERNER, ParseOutput, parse};
    pub use crate::token::{
        SemanticToken, SemanticTokens, Token, TokenPrinter, TokenSlice, Tokens,
    };
}
