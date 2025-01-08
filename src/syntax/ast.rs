use std::collections::BTreeMap;

use ecow::EcoString;

use super::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Num(f64),
    Char(char),
    Str(EcoString),
}

pub type Ident = EcoString;

#[derive(Clone, Debug, PartialEq)]
pub struct List {
    pub values: Vec<Expr>,
}

// { x = 10, y = 20 }
#[derive(Clone, Debug, PartialEq)]
pub struct Record {
    pub fields: BTreeMap<Ident, Expr>,
}

// x.y.z
#[derive(Clone, Debug, PartialEq)]
pub struct RecordSelect {
    pub source: Box<Expr>,
    pub field: Ident,
}

// following record operations can be combined with syntactic sugar:
// { y | +x = 10 | x = 100 | -x } == y

// { y | +x = 10 }
#[derive(Clone, Debug, PartialEq)]
pub struct RecordExtend {
    pub source: Box<Expr>,
    pub field: Ident,
    pub value: Box<Expr>,
}

// { y | -x }
#[derive(Clone, Debug, PartialEq)]
pub struct RecordRestrict {
    pub source: Box<Expr>,
    pub field: Ident,
}

// { y | x = 10 }
#[derive(Clone, Debug, PartialEq)]
pub struct RecordUpdate {
    pub source: Box<Expr>,
    pub field: Ident,
    pub value: Box<Expr>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub target: Box<Expr>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinaryOp {
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
    And,
    Or,
    Xor,
    // Equality
    Eq,
    NotEq,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetExpr {
    pub name: Ident,
    pub value: Box<Expr>,
    pub inside: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
    pub predicate: Box<Expr>,
    pub then: Box<Expr>,
    pub or: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordPat {
    pub fields: BTreeMap<Ident, Option<Pat>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pat {
    Error(Span),
    Wildcard(Span),
    Literal(Literal, Span),
    Ident(Ident, Span),
    Record(RecordPat, Span),
}

impl Pat {
    pub fn into_error(self) -> Option<Span> {
        match self {
            Self::Error(span) => Some(span),
            _ => None,
        }
    }

    pub fn into_wildcard(self) -> Option<Span> {
        match self {
            Self::Wildcard(span) => Some(span),
            _ => None,
        }
    }

    pub fn into_literal(self) -> Option<(Literal, Span)> {
        match self {
            Self::Literal(l, span) => Some((l, span)),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<(Ident, Span)> {
        match self {
            Self::Ident(i, span) => Some((i, span)),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<(RecordPat, Span)> {
        match self {
            Self::Record(r, span) => Some((r, span)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CaseExpr {
    pub source: Ident,
    pub branches: Vec<(Pat, Expr)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnExpr {
    pub param: Ident,
    pub body: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Error(Span),
    Literal(Literal, Span),
    Ident(Ident, Span),
    List(List, Span),
    Record(Record, Span),
    RecordSelect(RecordSelect, Span),
    RecordExtend(RecordExtend, Span),
    RecordRestrict(RecordRestrict, Span),
    RecordUpdate(RecordUpdate, Span),
    Unary(UnaryExpr, Span),
    Binary(BinaryExpr, Span),
    Let(LetExpr, Span),
    If(IfExpr, Span),
    Case(CaseExpr, Span),
    Fn(FnExpr, Span),
}

// TODO Function Call

impl Expr {
    pub fn into_literal(self) -> Option<(Literal, Span)> {
        match self {
            Self::Literal(l, span) => Some((l, span)),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<(Ident, Span)> {
        match self {
            Self::Ident(i, span) => Some((i, span)),
            _ => None,
        }
    }

    pub fn into_list(self) -> Option<(List, Span)> {
        match self {
            Self::List(l, span) => Some((l, span)),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<(Record, Span)> {
        match self {
            Self::Record(r, span) => Some((r, span)),
            _ => None,
        }
    }

    pub fn into_record_select(self) -> Option<(RecordSelect, Span)> {
        match self {
            Self::RecordSelect(s, span) => Some((s, span)),
            _ => None,
        }
    }

    pub fn into_record_extend(self) -> Option<(RecordExtend, Span)> {
        match self {
            Self::RecordExtend(e, span) => Some((e, span)),
            _ => None,
        }
    }

    pub fn into_record_update(self) -> Option<(RecordUpdate, Span)> {
        match self {
            Self::RecordUpdate(u, span) => Some((u, span)),
            _ => None,
        }
    }

    pub fn into_unary(self) -> Option<(UnaryExpr, Span)> {
        match self {
            Self::Unary(u, span) => Some((u, span)),
            _ => None,
        }
    }

    pub fn into_binary(self) -> Option<(BinaryExpr, Span)> {
        match self {
            Self::Binary(b, span) => Some((b, span)),
            _ => None,
        }
    }

    pub fn into_let(self) -> Option<(LetExpr, Span)> {
        match self {
            Self::Let(l, span) => Some((l, span)),
            _ => None,
        }
    }

    pub fn into_if(self) -> Option<(IfExpr, Span)> {
        match self {
            Self::If(i, span) => Some((i, span)),
            _ => None,
        }
    }

    pub fn into_case(self) -> Option<(CaseExpr, Span)> {
        match self {
            Self::Case(c, span) => Some((c, span)),
            _ => None,
        }
    }

    pub fn into_fn(self) -> Option<(FnExpr, Span)> {
        match self {
            Self::Fn(f, span) => Some((f, span)),
            _ => None,
        }
    }
}

// #[derive(Clone, Debug, PartialEq)]
// pub struct Binding {
//     pub pat: Pattern,
//     pub expr: Expr,
// }

// #[derive(Clone, Debug, PartialEq)]
// pub enum TypeExpr {
//     RecordExpr(Box<RecordExpr<'src, Self>>, Span),
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct TypeBinding {
//     pub ident: &'src str,
//     pub expr: TypeExpr,
// }

// #[derive(Clone, Debug, PartialEq)]
// pub struct Module {
//     pub types: Vec<TypeBinding>,
//     pub stmts: Vec<Binding>,
// }
