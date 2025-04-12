use bumpalo::collections::CollectIn;
use kola_print::prelude::*;
use owo_colors::OwoColorize;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Symbol,
    Operator,
    Literal,
    Keyword,
    Control,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Symbol => write!(f, "{}", "Symbol".yellow()),
            Self::Operator => write!(f, "{}", "Operator".green()),
            Self::Literal => write!(f, "{}", "Literal".purple()),
            Self::Keyword => write!(f, "{}", "Keyword".red()),
            Self::Control => write!(f, "{}", "Control".blue()),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Paren,
    Bracket,
    Brace,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Less,
    Greater,
    LessEq,
    GreaterEq,
    // Logical
    Not,
    And,
    Or,
    Xor,
    // Equality
    Eq,
    NotEq,
    // Record
    Merge,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Rem => write!(f, "%"),
            Self::Less => write!(f, "<"),
            Self::Greater => write!(f, ">"),
            Self::LessEq => write!(f, "<="),
            Self::GreaterEq => write!(f, ">="),
            Self::Not => write!(f, "!"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::Eq => write!(f, "=="),
            Self::NotEq => write!(f, "!="),
            Self::Merge => write!(f, "&"),
        }
    }
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
    Assign,
    Pipe,
    Backslash,
    Wildcard,
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
            Self::Assign => write!(f, "="),
            Self::Pipe => write!(f, "|"),
            Self::Backslash => write!(f, "\\"),
            Self::Arrow => write!(f, "->"),
            Self::DoubleArrow => write!(f, "=>"),
            Self::Wildcard => write!(f, "_"),
        }
    }
}

impl<'src> Token<'src> {
    pub fn kind(&self) -> TokenKind {
        match self {
            Self::Symbol(_) => TokenKind::Symbol,
            Self::Op(_) => TokenKind::Operator,
            Self::Num(_) | Self::Bool(_) | Self::Char(_) | Self::Str(_) => TokenKind::Literal,
            Self::Let
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
            | Self::Assign
            | Self::Pipe
            | Self::Backslash
            | Self::Arrow
            | Self::DoubleArrow
            | Self::Wildcard => TokenKind::Control,
        }
    }
}
