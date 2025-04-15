use bumpalo::collections::CollectIn;
use derive_more::Display;
use kola_print::prelude::*;
use std::fmt;

use super::span::Spanned;

pub struct TokenPrinter<'a>(pub &'a Tokens<'a>);

impl<'t> Printable<()> for TokenPrinter<'t> {
    fn notate<'a>(&'a self, _with: &'a (), arena: &'a Bump) -> Notation<'a> {
        let tokens = self
            .0
            .iter()
            .flat_map(|(token, span)| {
                let kind = token.kind();

                [
                    format_args!("\"{token}\"\t\t({kind}, {span})").display_in(arena),
                    arena.newline(),
                ]
            })
            .collect_in::<bumpalo::collections::Vec<_>>(arena);

        arena.concat(tokens.into_bump_slice())
    }
}

pub type Tokens<'a> = Vec<Spanned<Token<'a>>>;

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Symbol,
    Operator,
    Literal,
    Keyword,
    Control,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Delimiter {
    Paren,
    Bracket,
    Brace,
}

/*
Maybe also
<< bitwise left shift
>> bitwise right shift
^ xor
//=     flored divison
//
**=     Exponent
**

~ unary bitwise inversion ?
*/

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Op {
    #[display("=")]
    Assign,
    #[display("+")]
    Add,
    #[display("+=")]
    AddAssign,
    #[display("-")]
    Sub,
    #[display("-=")]
    SubAssign,
    #[display("*")]
    Mul,
    #[display("*=")]
    MulAssign,
    #[display("/")]
    Div,
    #[display("/=")]
    DivAssign,
    #[display("%")]
    Rem,
    #[display("%=")]
    RemAssign,
    // Comparison
    #[display("<")]
    Less,
    #[display(">")]
    Greater,
    #[display("<=")]
    LessEq,
    #[display(">=")]
    GreaterEq,
    // Logical
    #[display("!")]
    Not,
    #[display("and")]
    And,
    #[display("or")]
    Or,
    #[display("xor")]
    Xor,
    // Equality
    #[display("==")]
    Eq,
    #[display("!=")]
    NotEq,
    // Record
    #[display("&")]
    Merge,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Symbol(&'src str),
    Op(Op),
    // Literals
    Num(f64),
    Bool(bool),
    Char(char),
    Str(&'src str),
    // Keywords
    Type,
    Fn,
    Functor,
    Let,
    In,
    If,
    Then,
    Else,
    Case,
    Of,
    Forall,
    Import,
    Export,
    // Control
    Open(Delimiter),
    Close(Delimiter),
    Dot,
    Colon,
    Comma,
    Tilde,
    Pipe,
    Backslash,
    Underscore,
    Arrow,
    DoubleArrow,
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Op(op) => write!(f, "{op}"),
            Self::Num(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Char(c) => write!(f, "{c}"),
            Self::Str(s) => write!(f, "{s}"),
            // Keywords
            Self::Type => write!(f, "type"),
            Self::Fn => write!(f, "fn"),
            Self::Functor => write!(f, "functor"),
            Self::Let => write!(f, "let"),
            Self::In => write!(f, "in"),
            Self::If => write!(f, "if"),
            Self::Then => write!(f, "then"),
            Self::Else => write!(f, "else"),
            Self::Case => write!(f, "case"),
            Self::Of => write!(f, "of"),
            Self::Forall => write!(f, "forall"),
            Self::Import => write!(f, "import"),
            Self::Export => write!(f, "export"),
            // Control
            Self::Open(Delimiter::Paren) => write!(f, "("),
            Self::Open(Delimiter::Bracket) => write!(f, "["),
            Self::Open(Delimiter::Brace) => write!(f, "{{"),
            Self::Close(Delimiter::Paren) => write!(f, ")"),
            Self::Close(Delimiter::Bracket) => write!(f, "]"),
            Self::Close(Delimiter::Brace) => write!(f, "}}"),
            Self::Dot => write!(f, "."),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::Tilde => write!(f, "~"),
            Self::Pipe => write!(f, "|"),
            Self::Backslash => write!(f, "\\"),
            Self::Arrow => write!(f, "->"),
            Self::DoubleArrow => write!(f, "=>"),
            Self::Underscore => write!(f, "_"),
        }
    }
}

impl<'src> Token<'src> {
    pub fn kind(&self) -> TokenKind {
        match self {
            Self::Symbol(_) => TokenKind::Symbol,
            Self::Op(_) => TokenKind::Operator,
            Self::Num(_) | Self::Bool(_) | Self::Char(_) | Self::Str(_) => TokenKind::Literal,
            Self::Type
            | Self::Fn
            | Self::Functor
            | Self::Let
            | Self::In
            | Self::If
            | Self::Then
            | Self::Else
            | Self::Case
            | Self::Of
            | Self::Forall
            | Self::Import
            | Self::Export => TokenKind::Keyword,
            Self::Open(_)
            | Self::Close(_)
            | Self::Dot
            | Self::Colon
            | Self::Comma
            | Self::Tilde
            | Self::Pipe
            | Self::Backslash
            | Self::Arrow
            | Self::DoubleArrow
            | Self::Underscore => TokenKind::Control,
        }
    }
}
