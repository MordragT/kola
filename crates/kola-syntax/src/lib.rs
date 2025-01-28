use kola_tree::Phase;
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
    pub use crate::token::{Token, TokenKind, Tokens};
}

#[derive(Clone, Copy, Debug)]
pub struct SyntaxPhase;

impl Phase for SyntaxPhase {
    type Name = Span;
    type Ident = Span;
    type Literal = Span;
    type List = Span;
    type Property = Span;
    type Record = Span;
    type RecordSelect = Span;
    type RecordExtend = Span;
    type RecordRestrict = Span;
    type RecordUpdate = Span;
    type UnaryOp = Span;
    type Unary = Span;
    type BinaryOp = Span;
    type Binary = Span;
    type Let = Span;
    type PatError = Span;
    type Wildcard = Span;
    type LiteralPat = Span;
    type IdentPat = Span;
    type PropertyPat = Span;
    type RecordPat = Span;
    type Pat = Span;
    type Branch = Span;
    type Case = Span;
    type If = Span;
    type Func = Span;
    type Call = Span;
    type ExprError = Span;
    type Expr = Span;
}
