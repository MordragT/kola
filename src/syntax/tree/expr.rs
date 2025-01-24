use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{
    Binary, Call, Case, Func, Ident, If, Let, List, Literal, Metadata, Node, Record, RecordExtend,
    RecordRestrict, RecordSelect, RecordUpdate, Tree, Unary,
};
use crate::syntax::print::prelude::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExprError;

impl<M> Printable<Tree<M>> for ExprError {
    fn notate<'a>(&'a self, _with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        "ExprError".red().display_in(arena)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    Error(ExprError),
    Literal(Literal),
    Ident(Ident),
    List(List),
    Record(Record),
    RecordSelect(RecordSelect),
    RecordExtend(RecordExtend),
    RecordRestrict(RecordRestrict),
    RecordUpdate(RecordUpdate),
    Unary(Unary),
    Binary(Binary),
    Let(Let),
    If(If),
    Case(Case),
    Func(Func),
    Call(Call),
}

impl<M> Printable<Tree<M>> for Expr
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Error(e) => e.notate(with, arena),
            Self::Literal(l) => l.notate(with, arena),
            Self::Ident(i) => i.notate(with, arena),
            Self::List(l) => l.notate(with, arena),
            Self::Record(r) => r.notate(with, arena),
            Self::RecordSelect(r) => r.notate(with, arena),
            Self::RecordExtend(r) => r.notate(with, arena),
            Self::RecordRestrict(r) => r.notate(with, arena),
            Self::RecordUpdate(r) => r.notate(with, arena),
            Self::Unary(u) => u.notate(with, arena),
            Self::Binary(b) => b.notate(with, arena),
            Self::Let(l) => l.notate(with, arena),
            Self::If(i) => i.notate(with, arena),
            Self::Case(c) => c.notate(with, arena),
            Self::Func(f) => f.notate(with, arena),
            Self::Call(c) => c.notate(with, arena),
        }
    }
}

impl TryFrom<Node> for Expr {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, ()> {
        let expr = match value {
            Node::Ident(i) => Self::Ident(i),
            Node::Literal(l) => Self::Literal(l),
            Node::List(l) => Self::List(l),
            _ => return Err(()),
        };

        Ok(expr)
    }
}

// TODO Function Call

impl Expr {
    // pub fn ty(&self) -> Result<&MonoType, ExprError> {
    //     let ty = match self {
    //         Self::Error(e) => return Err(*e),
    //         Self::Literal(l) => l.ty(),
    //         Self::Ident(i) => i.ty(),
    //         Self::List(l) => l.ty(),
    //         Self::Record(r) => r.ty(),
    //         Self::RecordSelect(r) => r.ty(),
    //         Self::RecordExtend(r) => r.ty(),
    //         Self::RecordRestrict(r) => r.ty(),
    //         Self::RecordUpdate(r) => r.ty(),
    //         Self::Unary(u) => u.ty(),
    //         Self::Binary(b) => b.ty(),
    //         Self::Let(l) => l.ty(),
    //         Self::If(i) => i.ty(),
    //         Self::Case(c) => c.ty(),
    //         Self::Func(f) => f.ty(),
    //         Self::Call(c) => c.ty(),
    //     };

    //     Ok(ty)
    // }

    // pub fn ty_mut(&mut self) -> Result<&MonoType, ExprError> {
    //     let ty = match self {
    //         Self::Error(e) => return Err(*e),
    //         Self::Literal(l) => l.ty_mut(),
    //         Self::Ident(i) => i.ty_mut(),
    //         Self::List(l) => l.ty_mut(),
    //         Self::Record(r) => r.ty_mut(),
    //         Self::RecordSelect(r) => r.ty_mut(),
    //         Self::RecordExtend(r) => r.ty_mut(),
    //         Self::RecordRestrict(r) => r.ty_mut(),
    //         Self::RecordUpdate(r) => r.ty_mut(),
    //         Self::Unary(u) => u.ty_mut(),
    //         Self::Binary(b) => b.ty_mut(),
    //         Self::Let(l) => l.ty_mut(),
    //         Self::If(i) => i.ty_mut(),
    //         Self::Case(c) => c.ty_mut(),
    //         Self::Func(f) => f.ty_mut(),
    //         Self::Call(c) => c.ty_mut(),
    //     };

    //     Ok(ty)
    // }

    pub fn as_error(&self) -> Option<&ExprError> {
        match self {
            Self::Error(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&Literal> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_list(&self) -> Option<&List> {
        match self {
            Self::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_record(&self) -> Option<&Record> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn as_record_select(&self) -> Option<&RecordSelect> {
        match self {
            Self::RecordSelect(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_record_extend(&self) -> Option<&RecordExtend> {
        match self {
            Self::RecordExtend(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_record_update(&self) -> Option<&RecordUpdate> {
        match self {
            Self::RecordUpdate(u) => Some(u),
            _ => None,
        }
    }

    pub fn as_unary(&self) -> Option<&Unary> {
        match self {
            Self::Unary(u) => Some(u),
            _ => None,
        }
    }

    pub fn as_binary(&self) -> Option<&Binary> {
        match self {
            Self::Binary(b) => Some(b),
            _ => None,
        }
    }

    pub fn as_let(&self) -> Option<&Let> {
        match self {
            Self::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_if(&self) -> Option<&If> {
        match self {
            Self::If(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_case(&self) -> Option<&Case> {
        match self {
            Self::Case(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<&Func> {
        match self {
            Self::Func(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_call(&self) -> Option<&Call> {
        match self {
            Self::Call(c) => Some(c),
            _ => None,
        }
    }

    pub fn into_error(self) -> Option<ExprError> {
        match self {
            Self::Error(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_literal(self) -> Option<Literal> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Ident> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_list(self) -> Option<List> {
        match self {
            Self::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<Record> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn into_record_select(self) -> Option<RecordSelect> {
        match self {
            Self::RecordSelect(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_record_extend(self) -> Option<RecordExtend> {
        match self {
            Self::RecordExtend(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_record_update(self) -> Option<RecordUpdate> {
        match self {
            Self::RecordUpdate(u) => Some(u),
            _ => None,
        }
    }

    pub fn into_unary(self) -> Option<Unary> {
        match self {
            Self::Unary(u) => Some(u),
            _ => None,
        }
    }

    pub fn into_binary(self) -> Option<Binary> {
        match self {
            Self::Binary(b) => Some(b),
            _ => None,
        }
    }

    pub fn into_let(self) -> Option<Let> {
        match self {
            Self::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_if(self) -> Option<If> {
        match self {
            Self::If(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_case(self) -> Option<Case> {
        match self {
            Self::Case(c) => Some(c),
            _ => None,
        }
    }

    pub fn into_func(self) -> Option<Func> {
        match self {
            Self::Func(f) => Some(f),
            _ => None,
        }
    }

    pub fn into_call(self) -> Option<Call> {
        match self {
            Self::Call(c) => Some(c),
            _ => None,
        }
    }
}

impl From<Ident> for Expr {
    fn from(value: Ident) -> Self {
        Self::Ident(value)
    }
}

impl From<Literal> for Expr {
    fn from(value: Literal) -> Self {
        Self::Literal(value)
    }
}

impl From<List> for Expr {
    fn from(value: List) -> Self {
        Self::List(value)
    }
}

impl From<Record> for Expr {
    fn from(value: Record) -> Self {
        Self::Record(value)
    }
}

impl From<RecordSelect> for Expr {
    fn from(value: RecordSelect) -> Self {
        Self::RecordSelect(value)
    }
}

impl From<RecordExtend> for Expr {
    fn from(value: RecordExtend) -> Self {
        Self::RecordExtend(value)
    }
}

impl From<RecordRestrict> for Expr {
    fn from(value: RecordRestrict) -> Self {
        Self::RecordRestrict(value)
    }
}

impl From<RecordUpdate> for Expr {
    fn from(value: RecordUpdate) -> Self {
        Self::RecordUpdate(value)
    }
}

impl From<Unary> for Expr {
    fn from(value: Unary) -> Self {
        Self::Unary(value)
    }
}

impl From<Binary> for Expr {
    fn from(value: Binary) -> Self {
        Self::Binary(value)
    }
}

impl From<Let> for Expr {
    fn from(value: Let) -> Self {
        Self::Let(value)
    }
}

impl From<If> for Expr {
    fn from(value: If) -> Self {
        Self::If(value)
    }
}

impl From<Case> for Expr {
    fn from(value: Case) -> Self {
        Self::Case(value)
    }
}

impl From<Func> for Expr {
    fn from(value: Func) -> Self {
        Self::Func(value)
    }
}

impl From<Call> for Expr {
    fn from(value: Call) -> Self {
        Self::Call(value)
    }
}
