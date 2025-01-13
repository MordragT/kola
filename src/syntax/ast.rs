use std::collections::BTreeMap;

use ecow::EcoString;

use crate::node::Node;

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
pub struct List<M> {
    pub values: Vec<Expr<M>>,
}

// { x = 10, y = 20 }
#[derive(Clone, Debug, PartialEq)]
pub struct Record<M> {
    pub fields: BTreeMap<Ident, Expr<M>>,
}

// x.y.z
#[derive(Clone, Debug, PartialEq)]
pub struct RecordSelect<M> {
    pub source: Box<Expr<M>>,
    pub field: Ident,
}

// following record operations can be combined with syntactic sugar:
// { y | +x = 10 | x = 100 | -x } == y

// { y | +x = 10 }
#[derive(Clone, Debug, PartialEq)]
pub struct RecordExtend<M> {
    pub source: Box<Expr<M>>,
    pub field: Ident,
    pub value: Box<Expr<M>>,
}

// { y | -x }
#[derive(Clone, Debug, PartialEq)]
pub struct RecordRestrict<M> {
    pub source: Box<Expr<M>>,
    pub field: Ident,
}

// { y | x = 10 }
#[derive(Clone, Debug, PartialEq)]
pub struct RecordUpdate<M> {
    pub source: Box<Expr<M>>,
    pub field: Ident,
    pub value: Box<Expr<M>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr<M> {
    pub op: UnaryOp,
    pub target: Box<Expr<M>>,
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
pub struct BinaryExpr<M> {
    pub op: BinaryOp,
    pub left: Box<Expr<M>>,
    pub right: Box<Expr<M>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetExpr<M> {
    pub name: Ident,
    pub value: Box<Expr<M>>,
    pub inside: Box<Expr<M>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr<M> {
    pub predicate: Box<Expr<M>>,
    pub then: Box<Expr<M>>,
    pub or: Box<Expr<M>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordPat<M> {
    pub fields: BTreeMap<Ident, Option<Pat<M>>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pat<M> {
    Error(Span),
    Wildcard(Span),
    Literal(Node<Literal, M>),
    Ident(Node<Ident, M>),
    Record(Node<RecordPat<M>, M>),
}

impl<M> Pat<M> {
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

    pub fn into_literal(self) -> Option<Node<Literal, M>> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Node<Ident, M>> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<Node<RecordPat<M>, M>> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }
}

impl<M> From<Node<Literal, M>> for Pat<M> {
    fn from(value: Node<Literal, M>) -> Self {
        Self::Literal(value)
    }
}

impl<M> From<Node<Ident, M>> for Pat<M> {
    fn from(value: Node<Ident, M>) -> Self {
        Self::Ident(value)
    }
}

impl<M> From<Node<RecordPat<M>, M>> for Pat<M> {
    fn from(value: Node<RecordPat<M>, M>) -> Self {
        Self::Record(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CaseExpr<M> {
    pub source: Ident,
    pub branches: Vec<(Pat<M>, Expr<M>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnExpr<M> {
    pub param: Ident,
    pub body: Box<Expr<M>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallExpr<M> {
    pub func: Node<Ident, M>,
    pub arg: Box<Expr<M>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<M> {
    Error(Span),
    Literal(Node<Literal, M>),
    Ident(Node<Ident, M>),
    List(Node<List<M>, M>),
    Record(Node<Record<M>, M>),
    RecordSelect(Node<RecordSelect<M>, M>),
    RecordExtend(Node<RecordExtend<M>, M>),
    RecordRestrict(Node<RecordRestrict<M>, M>),
    RecordUpdate(Node<RecordUpdate<M>, M>),
    Unary(Node<UnaryExpr<M>, M>),
    Binary(Node<BinaryExpr<M>, M>),
    Let(Node<LetExpr<M>, M>),
    If(Node<IfExpr<M>, M>),
    Case(Node<CaseExpr<M>, M>),
    Fn(Node<FnExpr<M>, M>),
    Call(Node<CallExpr<M>, M>),
}

// TODO Function Call

impl<M> Expr<M> {
    pub fn into_literal(self) -> Option<Node<Literal, M>> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Node<Ident, M>> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_list(self) -> Option<Node<List<M>, M>> {
        match self {
            Self::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<Node<Record<M>, M>> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn into_record_select(self) -> Option<Node<RecordSelect<M>, M>> {
        match self {
            Self::RecordSelect(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_record_extend(self) -> Option<Node<RecordExtend<M>, M>> {
        match self {
            Self::RecordExtend(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_record_update(self) -> Option<Node<RecordUpdate<M>, M>> {
        match self {
            Self::RecordUpdate(u) => Some(u),
            _ => None,
        }
    }

    pub fn into_unary(self) -> Option<Node<UnaryExpr<M>, M>> {
        match self {
            Self::Unary(u) => Some(u),
            _ => None,
        }
    }

    pub fn into_binary(self) -> Option<Node<BinaryExpr<M>, M>> {
        match self {
            Self::Binary(b) => Some(b),
            _ => None,
        }
    }

    pub fn into_let(self) -> Option<Node<LetExpr<M>, M>> {
        match self {
            Self::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_if(self) -> Option<Node<IfExpr<M>, M>> {
        match self {
            Self::If(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_case(self) -> Option<Node<CaseExpr<M>, M>> {
        match self {
            Self::Case(c) => Some(c),
            _ => None,
        }
    }

    pub fn into_fn(self) -> Option<Node<FnExpr<M>, M>> {
        match self {
            Self::Fn(f) => Some(f),
            _ => None,
        }
    }

    pub fn into_call(self) -> Option<Node<CallExpr<M>, M>> {
        match self {
            Self::Call(c) => Some(c),
            _ => None,
        }
    }
}

impl<M> From<Node<Literal, M>> for Expr<M> {
    fn from(value: Node<Literal, M>) -> Self {
        Self::Literal(value)
    }
}

impl<M> From<Node<Ident, M>> for Expr<M> {
    fn from(value: Node<Ident, M>) -> Self {
        Self::Ident(value)
    }
}

impl<M> From<Node<List<M>, M>> for Expr<M> {
    fn from(value: Node<List<M>, M>) -> Self {
        Self::List(value)
    }
}

impl<M> From<Node<Record<M>, M>> for Expr<M> {
    fn from(value: Node<Record<M>, M>) -> Self {
        Self::Record(value)
    }
}

impl<M> From<Node<RecordSelect<M>, M>> for Expr<M> {
    fn from(value: Node<RecordSelect<M>, M>) -> Self {
        Self::RecordSelect(value)
    }
}

impl<M> From<Node<RecordExtend<M>, M>> for Expr<M> {
    fn from(value: Node<RecordExtend<M>, M>) -> Self {
        Self::RecordExtend(value)
    }
}

impl<M> From<Node<RecordRestrict<M>, M>> for Expr<M> {
    fn from(value: Node<RecordRestrict<M>, M>) -> Self {
        Self::RecordRestrict(value)
    }
}

impl<M> From<Node<RecordUpdate<M>, M>> for Expr<M> {
    fn from(value: Node<RecordUpdate<M>, M>) -> Self {
        Self::RecordUpdate(value)
    }
}

impl<M> From<Node<UnaryExpr<M>, M>> for Expr<M> {
    fn from(value: Node<UnaryExpr<M>, M>) -> Self {
        Self::Unary(value)
    }
}

impl<M> From<Node<BinaryExpr<M>, M>> for Expr<M> {
    fn from(value: Node<BinaryExpr<M>, M>) -> Self {
        Self::Binary(value)
    }
}

impl<M> From<Node<LetExpr<M>, M>> for Expr<M> {
    fn from(value: Node<LetExpr<M>, M>) -> Self {
        Self::Let(value)
    }
}

impl<M> From<Node<IfExpr<M>, M>> for Expr<M> {
    fn from(value: Node<IfExpr<M>, M>) -> Self {
        Self::If(value)
    }
}

impl<M> From<Node<CaseExpr<M>, M>> for Expr<M> {
    fn from(value: Node<CaseExpr<M>, M>) -> Self {
        Self::Case(value)
    }
}

impl<M> From<Node<FnExpr<M>, M>> for Expr<M> {
    fn from(value: Node<FnExpr<M>, M>) -> Self {
        Self::Fn(value)
    }
}

impl<M> From<Node<CallExpr<M>, M>> for Expr<M> {
    fn from(value: Node<CallExpr<M>, M>) -> Self {
        Self::Call(value)
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
