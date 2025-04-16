use bumpalo::collections::CollectIn;
use derive_more::Display;
use kola_print::prelude::*;
use std::fmt;

use super::span::Spanned;

// pub struct TokenPrinter<'t>(pub &'t Tokens<'t>);

// impl<'t> Printable<()> for TokenPrinter<'t> {
//     fn notate<'t>(&'t self, _with: &'t (), arena: &'t Bump) -> Notation<'t> {
//         let tokens = self
//             .0
//             .iter()
//             .flat_map(|(token, span)| {
//                 let kind = token.kind();

//                 [
//                     format_args!("\"{token}\"\t\t({kind}, {span})").display_in(arena),
//                     arena.newline(),
//                 ]
//             })
//             .collect_in::<bumpalo::collections::Vec<_>>(arena);

//         arena.concat(tokens.into_bump_slice())
//     }
// }

/*
Idea:

Make the SemanticToken owned (string slices to owned EcoString)
and then create a new tree module for the SemanticParseTree
where every node is just a collection of SemanticTokens
then create a new semantic_token_cache in the ParserState,
and create new combinators which operate with the newtype Token's,
which then push to the cache.
When a new node is inserted in to the SyntaxTree drain the cache
and also create a node in the SemanticParseTree (TODO: some nodes might not push at all to the cache, maybe work with indices or Option)
Therefore the NodeId's from the SyntaxTree can be reused for the
SemanticParseTree, and for syntax highlighting, it can then
be determined with the help of the SyntaxTree and SemanticParseTree
how the highlighting should look like.
To make the SemanticParseTree more powerful, the SemanticToken
should be more complex and better differentiate for example
between TypeNames or ModuleNames.
Essentially all the leaf nodes of the SyntaxTree should be incorporated.
Actually it might be possible to use newtype structs over leave nodes.
*/

#[derive(Debug, Display, Clone, Copy, PartialEq)]
pub enum Literal<'t> {
    Num(f64),
    Bool(bool),
    Char(char),
    Str(&'t str),
}

pub type Tokens<'t> = Vec<Spanned<Token<'t>>>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<'t> {
    Symbol(&'t str),
    Literal(Literal<'t>),
    Keyword(&'static str),
    Punct(char),
    Joint(char, char),
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct OpenT<'t>(pub Token<'t>);

impl<'t> OpenT<'t> {
    pub const PAREN: Self = Self(Token::Punct('('));
    pub const BRACKET: Self = Self(Token::Punct('['));
    pub const BRACE: Self = Self(Token::Punct('{'));
    pub const ANGLE: Self = Self(Token::Punct('<'));
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct CloseT<'t>(pub Token<'t>);

impl<'t> CloseT<'t> {
    pub const PAREN: Self = Self(Token::Punct(')'));
    pub const BRACKET: Self = Self(Token::Punct(']'));
    pub const BRACE: Self = Self(Token::Punct('}'));
    pub const ANGLE: Self = Self(Token::Punct('>'));
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct OpT<'t>(pub Token<'t>);

impl<'t> OpT<'t> {
    pub const ASSIGN: Self = Self(Token::Punct('='));
    pub const ADD: Self = Self(Token::Punct('+'));
    pub const ADD_ASSIGN: Self = Self(Token::Joint('+', '='));
    pub const SUB: Self = Self(Token::Punct('-'));
    pub const SUB_ASSIGN: Self = Self(Token::Joint('-', '='));
    pub const MUL: Self = Self(Token::Punct('*'));
    pub const MUL_ASSIGN: Self = Self(Token::Joint('*', '='));
    pub const DIV: Self = Self(Token::Punct('/'));
    pub const DIV_ASSIGN: Self = Self(Token::Joint('/', '='));
    pub const REM: Self = Self(Token::Punct('%'));
    pub const REM_ASSIGN: Self = Self(Token::Joint('%', '='));
    pub const LESS: Self = Self(Token::Punct('<'));
    pub const GREATER: Self = Self(Token::Punct('>'));
    pub const LESS_EQ: Self = Self(Token::Joint('<', '='));
    pub const GREATER_EQ: Self = Self(Token::Joint('>', '='));
    pub const NOT: Self = Self(Token::Punct('!'));
    pub const AND: Self = Self(Token::Keyword("and"));
    pub const OR: Self = Self(Token::Keyword("or"));
    pub const XOR: Self = Self(Token::Keyword("xor"));
    pub const EQ: Self = Self(Token::Joint('=', '='));
    pub const NOT_EQ: Self = Self(Token::Joint('!', '='));
    pub const MERGE: Self = Self(Token::Punct('&'));
}

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

impl<'t> From<OpT<'t>> for Op {
    fn from(value: OpT<'t>) -> Self {
        match value {
            op if op == OpT::ASSIGN => Op::Assign,
            op if op == OpT::ADD => Op::Add,
            op if op == OpT::ADD_ASSIGN => Op::AddAssign,
            op if op == OpT::SUB => Op::Sub,
            op if op == OpT::SUB_ASSIGN => Op::SubAssign,
            op if op == OpT::MUL => Op::Mul,
            op if op == OpT::MUL_ASSIGN => Op::MulAssign,
            op if op == OpT::DIV => Op::Div,
            op if op == OpT::DIV_ASSIGN => Op::DivAssign,
            op if op == OpT::REM => Op::Rem,
            op if op == OpT::REM_ASSIGN => Op::RemAssign,
            op if op == OpT::LESS => Op::Less,
            op if op == OpT::GREATER => Op::Greater,
            op if op == OpT::LESS_EQ => Op::LessEq,
            op if op == OpT::GREATER_EQ => Op::GreaterEq,
            op if op == OpT::NOT => Op::Not,
            op if op == OpT::AND => Op::And,
            op if op == OpT::OR => Op::Or,
            op if op == OpT::XOR => Op::Xor,
            op if op == OpT::EQ => Op::Eq,
            op if op == OpT::NOT_EQ => Op::NotEq,
            op if op == OpT::MERGE => Op::Merge,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct KwT<'t>(pub Token<'t>);

impl<'t> KwT<'t> {
    pub const TYPE: Self = Self(Token::Keyword("type"));
    pub const FN: Self = Self(Token::Keyword("fn"));
    pub const FUNCTOR: Self = Self(Token::Keyword("functor"));
    pub const LET: Self = Self(Token::Keyword("let"));
    pub const IN: Self = Self(Token::Keyword("in"));
    pub const IF: Self = Self(Token::Keyword("if"));
    pub const THEN: Self = Self(Token::Keyword("then"));
    pub const ELSE: Self = Self(Token::Keyword("else"));
    pub const CASE: Self = Self(Token::Keyword("case"));
    pub const OF: Self = Self(Token::Keyword("of"));
    pub const FORALL: Self = Self(Token::Keyword("forall"));
    pub const IMPORT: Self = Self(Token::Keyword("import"));
    pub const EXPORT: Self = Self(Token::Keyword("export"));
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
pub enum Kw {
    #[display("type")]
    Type,
    #[display("fn")]
    Fn,
    #[display("functor")]
    Functor,
    #[display("let")]
    Let,
    #[display("in")]
    In,
    #[display("if")]
    If,
    #[display("then")]
    Then,
    #[display("else")]
    Else,
    #[display("case")]
    Case,
    #[display("of")]
    Of,
    #[display("forall")]
    Forall,
    #[display("import")]
    Import,
    #[display("export")]
    Export,
}

impl<'t> From<KwT<'t>> for Kw {
    fn from(value: KwT<'t>) -> Self {
        match value {
            kw if kw == KwT::TYPE => Kw::Type,
            kw if kw == KwT::FN => Kw::Fn,
            kw if kw == KwT::FUNCTOR => Kw::Functor,
            kw if kw == KwT::LET => Kw::Let,
            kw if kw == KwT::IN => Kw::In,
            kw if kw == KwT::IF => Kw::If,
            kw if kw == KwT::THEN => Kw::Then,
            kw if kw == KwT::ELSE => Kw::Else,
            kw if kw == KwT::CASE => Kw::Case,
            kw if kw == KwT::OF => Kw::Of,
            kw if kw == KwT::FORALL => Kw::Forall,
            kw if kw == KwT::IMPORT => Kw::Import,
            kw if kw == KwT::EXPORT => Kw::Export,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct CtrlT<'t>(pub Token<'t>);

impl<'t> CtrlT<'t> {
    pub const DOT: Self = Self(Token::Punct('.'));
    pub const COLON: Self = Self(Token::Punct(':'));
    pub const COMMA: Self = Self(Token::Punct(','));
    pub const TILDE: Self = Self(Token::Punct('~'));
    pub const PIPE: Self = Self(Token::Punct('|'));
    pub const BACKSLASH: Self = Self(Token::Punct('\\'));
    pub const UNDERSCORE: Self = Self(Token::Punct('_'));
    pub const ARROW: Self = Self(Token::Joint('-', '>'));
    pub const DOUBLE_ARROW: Self = Self(Token::Joint('=', '>'));
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ctrl {
    #[display(".")]
    Dot,
    #[display(":")]
    Colon,
    #[display(",")]
    Comma,
    #[display("~")]
    Tilde,
    #[display("|")]
    Pipe,
    #[display("\\")]
    Backslash,
    #[display("_")]
    Underscore,
    #[display("->")]
    Arrow,
    #[display("=>")]
    DoubleArrow,
}

impl<'t> From<CtrlT<'t>> for Ctrl {
    fn from(value: CtrlT<'t>) -> Self {
        match value {
            c if c == CtrlT::DOT => Ctrl::Dot,
            c if c == CtrlT::COLON => Ctrl::Colon,
            c if c == CtrlT::COMMA => Ctrl::Comma,
            c if c == CtrlT::TILDE => Ctrl::Tilde,
            c if c == CtrlT::PIPE => Ctrl::Pipe,
            c if c == CtrlT::BACKSLASH => Ctrl::Backslash,
            c if c == CtrlT::UNDERSCORE => Ctrl::Underscore,
            c if c == CtrlT::ARROW => Ctrl::Arrow,
            c if c == CtrlT::DOUBLE_ARROW => Ctrl::DoubleArrow,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Delimiter {
    Paren,
    Bracket,
    Brace,
    Angle,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SemanticTokenKind {
    Symbol,
    Operator,
    Literal,
    Keyword,
    Control,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SemanticToken<'t> {
    Symbol(&'t str),
    Op(Op),
    Literal(Literal<'t>),
    Keyword(Kw),
    Open(Delimiter),
    Close(Delimiter),
    Control(Ctrl),
}

impl<'t> SemanticToken<'t> {
    pub fn kind(&self) -> SemanticTokenKind {
        match self {
            Self::Symbol(_) => SemanticTokenKind::Symbol,
            Self::Op(_) => SemanticTokenKind::Operator,
            Self::Literal(_) => SemanticTokenKind::Literal,
            Self::Keyword(_) => SemanticTokenKind::Keyword,
            Self::Open(_) | Self::Close(_) | Self::Control(_) => SemanticTokenKind::Control,
        }
    }
}

impl<'t> From<OpenT<'t>> for SemanticToken<'t> {
    fn from(value: OpenT<'t>) -> Self {
        if value == OpenT::PAREN {
            SemanticToken::Open(Delimiter::Paren)
        } else if value == OpenT::BRACKET {
            SemanticToken::Open(Delimiter::Bracket)
        } else if value == OpenT::BRACE {
            SemanticToken::Open(Delimiter::Brace)
        } else if value == OpenT::ANGLE {
            SemanticToken::Open(Delimiter::Angle)
        } else {
            unreachable!()
        }
    }
}

impl<'t> From<CloseT<'t>> for SemanticToken<'t> {
    fn from(value: CloseT<'t>) -> Self {
        if value == CloseT::PAREN {
            SemanticToken::Close(Delimiter::Paren)
        } else if value == CloseT::BRACKET {
            SemanticToken::Close(Delimiter::Bracket)
        } else if value == CloseT::BRACE {
            SemanticToken::Close(Delimiter::Brace)
        } else if value == CloseT::ANGLE {
            SemanticToken::Close(Delimiter::Angle)
        } else {
            unreachable!()
        }
    }
}

impl<'t> fmt::Display for SemanticToken<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Op(op) => write!(f, "{op}"),
            Self::Literal(l) => write!(f, "{l}"),
            Self::Keyword(kw) => write!(f, "{kw}"),
            Self::Open(Delimiter::Paren) => write!(f, "("),
            Self::Open(Delimiter::Bracket) => write!(f, "["),
            Self::Open(Delimiter::Brace) => write!(f, "{{"),
            Self::Open(Delimiter::Angle) => write!(f, "<"),
            Self::Close(Delimiter::Paren) => write!(f, ")"),
            Self::Close(Delimiter::Bracket) => write!(f, "]"),
            Self::Close(Delimiter::Brace) => write!(f, "}}"),
            Self::Close(Delimiter::Angle) => write!(f, ">"),
            Self::Control(c) => write!(f, "{c}"),
        }
    }
}

impl<'t> From<OpT<'t>> for SemanticToken<'t> {
    fn from(value: OpT<'t>) -> Self {
        Self::Op(value.into())
    }
}

impl<'t> From<KwT<'t>> for SemanticToken<'t> {
    fn from(value: KwT<'t>) -> Self {
        Self::Keyword(value.into())
    }
}

impl<'t> From<CtrlT<'t>> for SemanticToken<'t> {
    fn from(value: CtrlT<'t>) -> Self {
        Self::Control(value.into())
    }
}

impl<'t> From<Op> for SemanticToken<'t> {
    fn from(value: Op) -> Self {
        Self::Op(value)
    }
}

impl<'t> From<Kw> for SemanticToken<'t> {
    fn from(value: Kw) -> Self {
        Self::Keyword(value)
    }
}

impl<'t> From<Ctrl> for SemanticToken<'t> {
    fn from(value: Ctrl) -> Self {
        Self::Control(value)
    }
}

impl<'t> From<Literal<'t>> for SemanticToken<'t> {
    fn from(value: Literal<'t>) -> Self {
        Self::Literal(value)
    }
}
