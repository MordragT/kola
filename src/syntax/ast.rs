use std::collections::BTreeMap;

use super::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal<'src> {
    Bool(bool),
    Num(f64),
    Char(char),
    Str(&'src str),
}

pub type Ident<'src> = &'src str;
pub type List<'src> = Vec<Expr<'src>>;
pub type Record<'src> = BTreeMap<Ident<'src>, Expr<'src>>;
pub type Func<'src> = Box<Binding<'src>>;

#[derive(Clone, Debug, PartialEq)]
pub enum RecordExpr<'src, E> {
    // { x, y }.x
    // source field
    Selection(E, Ident<'src>),
    // { +x = 10 | y }
    // source, field, value
    Extension(E, Ident<'src>, E),
    // { -x | y }
    // source field
    Restriction(E, Ident<'src>),
    // { x = 10 | y }
    // source, field, value
    Update(E, Ident<'src>, E),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'src> {
    Error(Span),
    Literal(Literal<'src>, Span),
    Ident(Ident<'src>, Span),
    List(List<'src>, Span),
    Func(Func<'src>, Span),
    Record(Record<'src>, Span),
    RecordExpr(Box<RecordExpr<'src, Self>>, Span),
}

impl<'src> Expr<'src> {
    pub fn into_literal(self) -> Option<(Literal<'src>, Span)> {
        match self {
            Self::Literal(l, span) => Some((l, span)),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<(Ident<'src>, Span)> {
        match self {
            Self::Ident(i, span) => Some((i, span)),
            _ => None,
        }
    }

    pub fn into_list(self) -> Option<(List<'src>, Span)> {
        match self {
            Self::List(l, span) => Some((l, span)),
            _ => None,
        }
    }

    pub fn into_func(self) -> Option<(Func<'src>, Span)> {
        match self {
            Self::Func(f, span) => Some((f, span)),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<(Record<'src>, Span)> {
        match self {
            Self::Record(r, span) => Some((r, span)),
            _ => None,
        }
    }

    pub fn into_record_expr(self) -> Option<(RecordExpr<'src, Self>, Span)> {
        match self {
            Self::RecordExpr(e, span) => Some((*e, span)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'src> {
    Wildcard,
    Ident(&'src str),
    // do not allow nested destructuring ??
    Record(Vec<&'src str>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binding<'src> {
    pub pat: Pattern<'src>,
    pub expr: Expr<'src>,
}

pub type RecordTy<'src> = BTreeMap<Ident<'src>, TypeExpr<'src>>;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeExpr<'src> {
    Record(RecordTy<'src>, Span),
    RecordExpr(Box<RecordExpr<'src, Self>>, Span),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeBinding<'src> {
    pub ident: &'src str,
    pub expr: TypeExpr<'src>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module<'src> {
    pub types: Vec<TypeBinding<'src>>,
    pub stmts: Vec<Binding<'src>>,
}
