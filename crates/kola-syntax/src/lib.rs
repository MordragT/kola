#![feature(const_trait_impl)]
#![feature(const_convert)]
#![feature(const_cmp)]
#![feature(derive_const)]
#![feature(impl_trait_in_assoc_type)]

pub mod lexer;
pub mod loc;
pub mod parser;
pub mod token;

pub mod prelude {
    pub use crate::lexer::{LexInput, tokenize};
    pub use crate::loc::{LocDecorator, LocPhase, Locations};
    pub use crate::parser::{ParseInput, ParseOutput, make_input, parse};
    pub use crate::token::{
        SemanticToken, SemanticTokens, Token, TokenPrinter, TokenSlice, Tokens,
    };
}
