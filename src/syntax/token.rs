use std::fmt;

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
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Symbol(&'src str),
    Op(Op),
    Open(Delimiter),
    Close(Delimiter),
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
    Import,
    Export,
    // Control
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
            Self::Open(Delimiter::Paren) => write!(f, "("),
            Self::Open(Delimiter::Bracket) => write!(f, "["),
            Self::Open(Delimiter::Brace) => write!(f, "{{"),
            Self::Close(Delimiter::Paren) => write!(f, ")"),
            Self::Close(Delimiter::Bracket) => write!(f, "]"),
            Self::Close(Delimiter::Brace) => write!(f, "}}"),
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
            Self::Import => write!(f, "import"),
            Self::Export => write!(f, "export"),
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
