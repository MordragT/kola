use std::collections::BTreeMap;

use ecow::EcoString;

use super::{Span, Spanned};

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
    Literal(Spanned<Literal>),
    Ident(Spanned<Ident>),
    Record(Spanned<RecordPat>),
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

    pub fn into_literal(self) -> Option<Spanned<Literal>> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Spanned<Ident>> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<Spanned<RecordPat>> {
        match self {
            Self::Record(r) => Some(r),
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
pub struct CallExpr {
    pub func: Spanned<Ident>,
    pub arg: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Error(Span),
    Literal(Spanned<Literal>),
    Ident(Spanned<Ident>),
    List(Spanned<List>),
    Record(Spanned<Record>),
    RecordSelect(Spanned<RecordSelect>),
    RecordExtend(Spanned<RecordExtend>),
    RecordRestrict(Spanned<RecordRestrict>),
    RecordUpdate(Spanned<RecordUpdate>),
    Unary(Spanned<UnaryExpr>),
    Binary(Spanned<BinaryExpr>),
    Let(Spanned<LetExpr>),
    If(Spanned<IfExpr>),
    Case(Spanned<CaseExpr>),
    Fn(Spanned<FnExpr>),
    Call(Spanned<CallExpr>),
}

// TODO Function Call

impl Expr {
    pub fn into_literal(self) -> Option<Spanned<Literal>> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Spanned<Ident>> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_list(self) -> Option<Spanned<List>> {
        match self {
            Self::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<Spanned<Record>> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn into_record_select(self) -> Option<Spanned<RecordSelect>> {
        match self {
            Self::RecordSelect(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_record_extend(self) -> Option<Spanned<RecordExtend>> {
        match self {
            Self::RecordExtend(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_record_update(self) -> Option<Spanned<RecordUpdate>> {
        match self {
            Self::RecordUpdate(u) => Some(u),
            _ => None,
        }
    }

    pub fn into_unary(self) -> Option<Spanned<UnaryExpr>> {
        match self {
            Self::Unary(u) => Some(u),
            _ => None,
        }
    }

    pub fn into_binary(self) -> Option<Spanned<BinaryExpr>> {
        match self {
            Self::Binary(b) => Some(b),
            _ => None,
        }
    }

    pub fn into_let(self) -> Option<Spanned<LetExpr>> {
        match self {
            Self::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_if(self) -> Option<Spanned<IfExpr>> {
        match self {
            Self::If(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_case(self) -> Option<Spanned<CaseExpr>> {
        match self {
            Self::Case(c) => Some(c),
            _ => None,
        }
    }

    pub fn into_fn(self) -> Option<Spanned<FnExpr>> {
        match self {
            Self::Fn(f) => Some(f),
            _ => None,
        }
    }

    pub fn into_call(self) -> Option<Spanned<CallExpr>> {
        match self {
            Self::Call(c) => Some(c),
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
//     RecordExpr(Box<RecordExpr<'src, Self>>),
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
