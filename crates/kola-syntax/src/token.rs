use derive_more::Display;
use kola_utils::interner::PathKey;
use paste::paste;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt, ops::Index};

use kola_print::prelude::*;
use kola_span::Located;

pub struct TokenPrinter<'t>(pub &'t Tokens<'t>);

impl<'t> Printable<PrintOptions> for TokenPrinter<'t> {
    fn notate<'a>(&'a self, with: &'a PrintOptions, arena: &'a Bump) -> Notation<'a> {
        let tokens = self
            .0
            .iter()
            .map(|(token, span)| {
                let head_str = format!("\"{token}\"");
                let head = head_str.display_in(arena);

                let kind = match token {
                    Token::Atom(_) => "atom".red().display_in(arena),
                    Token::Symbol(_) => "symbol".blue().display_in(arena),
                    Token::Literal(_) => "literal".green().display_in(arena),
                };
                let span = span.display_in(arena);

                let rhs = [kind, ", ".display_in(arena), span]
                    .concat_in(arena)
                    .enclose(arena.just('('), arena.just(')'), arena);

                let spacing_width = (with.width as usize / 3)
                    .checked_sub(head_str.len())
                    .unwrap_or(1);
                let spacing = arena.just(' ').repeat(spacing_width, arena);

                let single = [spacing, rhs.clone()].concat_in(arena).flatten(arena);
                let multi = [arena.newline(), rhs].concat_in(arena).indent(arena);

                head.then(single.or(multi, arena), arena)
                    .then(arena.newline(), arena)
            })
            .collect_in::<bumpalo::collections::Vec<_>>(arena);

        arena.concat(tokens.into_bump_slice())
    }

    fn print(&self, options: PrintOptions)
    where
        PrintOptions: Default,
    {
        self.print_with(&options, options);
    }
}

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
pub enum LiteralT<'t> {
    Num(f64),
    Bool(bool),
    Char(char),
    Str(&'t str),
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct TokenCache<'t> {
    cache: HashMap<PathKey, Tokens<'t>>,
}

impl<'t> TokenCache<'t> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, path: &PathKey) -> Option<&Tokens> {
        self.cache.get(path)
    }

    pub fn get_mut(&mut self, path: &PathKey) -> Option<&mut Tokens<'t>> {
        self.cache.get_mut(path)
    }

    pub fn insert(&mut self, path: PathKey, tokens: Tokens<'t>) {
        self.cache.insert(path, tokens);
    }
}

impl<'t> Index<PathKey> for TokenCache<'t> {
    type Output = Tokens<'t>;

    fn index(&self, index: PathKey) -> &Self::Output {
        self.cache.get(&index).unwrap()
    }
}

pub type Tokens<'t> = Vec<Located<Token<'t>>>;
pub type TokenSlice<'t> = &'t [Located<Token<'t>>];

#[derive(Debug, Display, Clone, Copy, PartialEq)]
pub enum Token<'t> {
    #[display("{_0}")]
    Atom(&'static str),
    #[display("{_0}")]
    Symbol(&'t str),
    #[display("{_0}")]
    Literal(LiteralT<'t>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct OpenT<'t>(pub Token<'t>);

impl<'t> OpenT<'t> {
    pub const PAREN: Self = Self(Token::Atom("("));
    pub const BRACKET: Self = Self(Token::Atom("["));
    pub const BRACE: Self = Self(Token::Atom("{"));
    pub const ANGLE: Self = Self(Token::Atom("<"));
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct CloseT<'t>(pub Token<'t>);

impl<'t> CloseT<'t> {
    pub const PAREN: Self = Self(Token::Atom(")"));
    pub const BRACKET: Self = Self(Token::Atom("]"));
    pub const BRACE: Self = Self(Token::Atom("}"));
    pub const ANGLE: Self = Self(Token::Atom(">"));
}

macro_rules! define_token_type {
    (
        $name:ident {
            $(
                $variant:ident => $atom:literal
            ),* $(,)?
        }
    ) => {
        paste!{
            // Define the wrapper struct (OpT, KwT, CtrlT, etc.)
            #[derive(Clone, Copy, Debug, PartialEq)]
            #[non_exhaustive]
            pub struct [<$name T>]<'t>(pub Token<'t>);

            impl<'t> [<$name T>]<'t> {
                $(pub const [<$variant:snake:upper>]: Self = Self(Token::Atom($atom));)*
            }

            // Define the semantic enum (Op, Kw, Ctrl, etc.)
            #[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
            pub enum $name {
                $(
                    #[display($atom)]
                    $variant,
                )*
            }

            // Implement conversion from wrapper to semantic type
            impl<'t> From<[<$name T>]<'t>> for $name {
                fn from(value: [<$name T>]<'t>) -> Self {
                    match value {
                        $(v if v == [<$name T>]::[<$variant:snake:upper>] => $name::$variant,)*
                        _ => unreachable!(),
                    }
                }
            }

            // Implement conversion from semantic type to SemanticToken
            impl<'t> From<$name> for SemanticToken {
                fn from(value: $name) -> Self {
                    Self::$name(value)
                }
            }

            // Implement conversion from wrapper to SemanticToken
            impl<'t> From<[<$name T>]<'t>> for SemanticToken {
                fn from(value: [<$name T>]<'t>) -> Self {
                    Self::$name(value.into())
                }
            }
        }
    };
}

define_token_type!(Op {
    Assign => "=",
    Add => "+",
    AddAssign => "+=",
    Sub => "-",
    SubAssign => "-=",
    Mul => "*",
    MulAssign => "*=",
    Div => "/",
    DivAssign => "/=",
    Rem => "%",
    RemAssign => "%=",
    // Comparison
    Less => "<",
    Greater => ">",
    LessEq => "<=",
    GreaterEq => ">=",
    // Logical
    Not => "!",
    And => "and",
    Or => "or",
    Xor => "xor",
    // Equality
    Eq => "==",
    NotEq => "!=",
    // Record
    Merge => "&",
});

define_token_type!(Kw {
    Module => "module",
    Import => "import",
    Export => "export",
    Functor => "functor",
    Type => "type",
    Forall => "forall",
    Fn => "fn",
    Let => "let",
    In => "in",
    If => "if",
    Then => "then",
    Else => "else",
    Case => "case",
    Of => "of",
});

define_token_type!(Ctrl {
    Dot => ".",
    Colon => ":",
    Comma => ",",
    Tilde => "~",
    Pipe => "|",
    Backslash => "\\",
    Underscore => "_",
    Arrow => "->",
    DoubleArrow => "=>",
});

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Delim {
    Paren,
    Bracket,
    Brace,
    Angle,
}

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum DelimSide {
    Open,
    Close,
}

pub type SemanticTokens = Vec<Located<SemanticToken>>;

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Literal {
    Num,
    Bool,
    Char,
    Str,
}

impl<'t> From<LiteralT<'t>> for Literal {
    fn from(value: LiteralT<'t>) -> Self {
        match value {
            LiteralT::Num(_) => Literal::Num,
            LiteralT::Str(_) => Literal::Str,
            LiteralT::Bool(_) => Literal::Bool,
            LiteralT::Char(_) => Literal::Char,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum SemanticToken {
    Symbol,
    Literal(Literal),
    Kw(Kw),
    Op(Op),
    Ctrl(Ctrl),
    Open(Delim),
    Close(Delim),
}

impl<'t> From<OpenT<'t>> for SemanticToken {
    fn from(value: OpenT<'t>) -> Self {
        if value == OpenT::PAREN {
            SemanticToken::Open(Delim::Paren)
        } else if value == OpenT::BRACKET {
            SemanticToken::Open(Delim::Bracket)
        } else if value == OpenT::BRACE {
            SemanticToken::Open(Delim::Brace)
        } else if value == OpenT::ANGLE {
            SemanticToken::Open(Delim::Angle)
        } else {
            unreachable!()
        }
    }
}

impl<'t> From<CloseT<'t>> for SemanticToken {
    fn from(value: CloseT<'t>) -> Self {
        if value == CloseT::PAREN {
            SemanticToken::Close(Delim::Paren)
        } else if value == CloseT::BRACKET {
            SemanticToken::Close(Delim::Bracket)
        } else if value == CloseT::BRACE {
            SemanticToken::Close(Delim::Brace)
        } else if value == CloseT::ANGLE {
            SemanticToken::Close(Delim::Angle)
        } else {
            unreachable!()
        }
    }
}

impl<'t> From<LiteralT<'t>> for SemanticToken {
    fn from(value: LiteralT<'t>) -> Self {
        Self::Literal(value.into())
    }
}

impl<'t> fmt::Display for SemanticToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Symbol => write!(f, "Symbol"),
            Self::Literal(l) => write!(f, "{l}"),
            Self::Kw(kw) => write!(f, "{kw}"),
            Self::Op(op) => write!(f, "{op}"),
            Self::Ctrl(c) => write!(f, "{c}"),
            Self::Open(Delim::Paren) => write!(f, "("),
            Self::Open(Delim::Bracket) => write!(f, "["),
            Self::Open(Delim::Brace) => write!(f, "{{"),
            Self::Open(Delim::Angle) => write!(f, "<"),
            Self::Close(Delim::Paren) => write!(f, ")"),
            Self::Close(Delim::Bracket) => write!(f, "]"),
            Self::Close(Delim::Brace) => write!(f, "}}"),
            Self::Close(Delim::Angle) => write!(f, ">"),
        }
    }
}
