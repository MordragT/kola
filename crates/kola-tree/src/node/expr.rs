use derive_more::From;
use kola_print::prelude::*;
use kola_utils::as_variant;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{
    Binary, Call, Case, Func, Ident, If, Let, List, Literal, Record, RecordExtend, RecordRestrict,
    RecordSelect, RecordUpdate, Unary,
};
use crate::{id::NodeId, print::TreePrinter};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExprError;

impl Printable<TreePrinter> for ExprError {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        "ExprError".red().display_in(arena)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize, From)]
pub enum Expr {
    Error(ExprError),
    Literal(NodeId<Literal>),
    Ident(NodeId<Ident>),
    List(NodeId<List>),
    Record(NodeId<Record>),
    RecordSelect(NodeId<RecordSelect>),
    RecordExtend(NodeId<RecordExtend>),
    RecordRestrict(NodeId<RecordRestrict>),
    RecordUpdate(NodeId<RecordUpdate>),
    Unary(NodeId<Unary>),
    Binary(NodeId<Binary>),
    Let(NodeId<Let>),
    If(NodeId<If>),
    Case(NodeId<Case>),
    Func(NodeId<Func>),
    Call(NodeId<Call>),
}

impl Printable<TreePrinter> for Expr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Error(e) => e.notate(with, arena),
            Self::Literal(l) => l.get(&with.tree).notate(with, arena),
            Self::Ident(i) => i.get(&with.tree).notate(with, arena),
            Self::List(l) => l.get(&with.tree).notate(with, arena),
            Self::Record(r) => r.get(&with.tree).notate(with, arena),
            Self::RecordSelect(r) => r.get(&with.tree).notate(with, arena),
            Self::RecordExtend(r) => r.get(&with.tree).notate(with, arena),
            Self::RecordRestrict(r) => r.get(&with.tree).notate(with, arena),
            Self::RecordUpdate(r) => r.get(&with.tree).notate(with, arena),
            Self::Unary(u) => u.get(&with.tree).notate(with, arena),
            Self::Binary(b) => b.get(&with.tree).notate(with, arena),
            Self::Let(l) => l.get(&with.tree).notate(with, arena),
            Self::If(i) => i.get(&with.tree).notate(with, arena),
            Self::Case(c) => c.get(&with.tree).notate(with, arena),
            Self::Func(f) => f.get(&with.tree).notate(with, arena),
            Self::Call(c) => c.get(&with.tree).notate(with, arena),
        }
    }
}

// TODO Function Call

impl Expr {
    pub fn as_error(&self) -> Option<&ExprError> {
        as_variant!(self, Self::Error)
    }

    pub fn as_literal(&self) -> Option<&NodeId<Literal>> {
        as_variant!(self, Self::Literal)
    }

    pub fn as_ident(&self) -> Option<&NodeId<Ident>> {
        as_variant!(self, Self::Ident)
    }

    pub fn as_list(&self) -> Option<&NodeId<List>> {
        as_variant!(self, Self::List)
    }

    pub fn as_record(&self) -> Option<&NodeId<Record>> {
        as_variant!(self, Self::Record)
    }

    pub fn as_record_select(&self) -> Option<&NodeId<RecordSelect>> {
        as_variant!(self, Self::RecordSelect)
    }

    pub fn as_record_extend(&self) -> Option<&NodeId<RecordExtend>> {
        as_variant!(self, Self::RecordExtend)
    }

    pub fn as_record_update(&self) -> Option<&NodeId<RecordUpdate>> {
        as_variant!(self, Self::RecordUpdate)
    }

    pub fn as_unary(&self) -> Option<&NodeId<Unary>> {
        as_variant!(self, Self::Unary)
    }

    pub fn as_binary(&self) -> Option<&NodeId<Binary>> {
        as_variant!(self, Self::Binary)
    }

    pub fn as_let(&self) -> Option<&NodeId<Let>> {
        as_variant!(self, Self::Let)
    }

    pub fn as_if(&self) -> Option<&NodeId<If>> {
        as_variant!(self, Self::If)
    }

    pub fn as_case(&self) -> Option<&NodeId<Case>> {
        as_variant!(self, Self::Case)
    }

    pub fn as_func(&self) -> Option<&NodeId<Func>> {
        as_variant!(self, Self::Func)
    }

    pub fn as_call(&self) -> Option<&NodeId<Call>> {
        as_variant!(self, Self::Call)
    }

    pub fn into_error(self) -> Option<ExprError> {
        as_variant!(self, Self::Error)
    }

    pub fn into_literal(self) -> Option<NodeId<Literal>> {
        as_variant!(self, Self::Literal)
    }

    pub fn into_ident(self) -> Option<NodeId<Ident>> {
        as_variant!(self, Self::Ident)
    }

    pub fn into_list(self) -> Option<NodeId<List>> {
        as_variant!(self, Self::List)
    }

    pub fn into_record(self) -> Option<NodeId<Record>> {
        as_variant!(self, Self::Record)
    }

    pub fn into_record_select(self) -> Option<NodeId<RecordSelect>> {
        as_variant!(self, Self::RecordSelect)
    }

    pub fn into_record_extend(self) -> Option<NodeId<RecordExtend>> {
        as_variant!(self, Self::RecordExtend)
    }

    pub fn into_record_update(self) -> Option<NodeId<RecordUpdate>> {
        as_variant!(self, Self::RecordUpdate)
    }

    pub fn into_unary(self) -> Option<NodeId<Unary>> {
        as_variant!(self, Self::Unary)
    }

    pub fn into_binary(self) -> Option<NodeId<Binary>> {
        as_variant!(self, Self::Binary)
    }

    pub fn into_let(self) -> Option<NodeId<Let>> {
        as_variant!(self, Self::Let)
    }

    pub fn into_if(self) -> Option<NodeId<If>> {
        as_variant!(self, Self::If)
    }

    pub fn into_case(self) -> Option<NodeId<Case>> {
        as_variant!(self, Self::Case)
    }

    pub fn into_func(self) -> Option<NodeId<Func>> {
        as_variant!(self, Self::Func)
    }

    pub fn into_call(self) -> Option<NodeId<Call>> {
        as_variant!(self, Self::Call)
    }
}

// impl From<NodeId<Ident>> for Expr {
//     fn from(value: NodeId<Ident>) -> Self {
//         Self::Ident(value)
//     }
// }

// impl From<NodeId<Literal>> for Expr {
//     fn from(value: NodeId<Literal>) -> Self {
//         Self::Literal(value)
//     }
// }

// impl From<NodeId<List>> for Expr {
//     fn from(value: NodeId<List>) -> Self {
//         Self::List(value)
//     }
// }

// impl From<NodeId<Record>> for Expr {
//     fn from(value: NodeId<Record>) -> Self {
//         Self::Record(value)
//     }
// }

// impl From<NodeId<RecordSelect>> for Expr {
//     fn from(value: NodeId<RecordSelect>) -> Self {
//         Self::RecordSelect(value)
//     }
// }

// impl From<NodeId<RecordExtend>> for Expr {
//     fn from(value: NodeId<RecordExtend>) -> Self {
//         Self::RecordExtend(value)
//     }
// }

// impl From<NodeId<RecordRestrict>> for Expr {
//     fn from(value: NodeId<RecordRestrict>) -> Self {
//         Self::RecordRestrict(value)
//     }
// }

// impl From<NodeId<RecordUpdate>> for Expr {
//     fn from(value: NodeId<RecordUpdate>) -> Self {
//         Self::RecordUpdate(value)
//     }
// }

// impl From<NodeId<Unary>> for Expr {
//     fn from(value: NodeId<Unary>) -> Self {
//         Self::Unary(value)
//     }
// }

// impl From<NodeId<Binary>> for Expr {
//     fn from(value: NodeId<Binary>) -> Self {
//         Self::Binary(value)
//     }
// }

// impl From<NodeId<Let>> for Expr {
//     fn from(value: NodeId<Let>) -> Self {
//         Self::Let(value)
//     }
// }

// impl From<NodeId<If>> for Expr {
//     fn from(value: NodeId<If>) -> Self {
//         Self::If(value)
//     }
// }

// impl From<NodeId<Case>> for Expr {
//     fn from(value: NodeId<Case>) -> Self {
//         Self::Case(value)
//     }
// }

// impl From<NodeId<Func>> for Expr {
//     fn from(value: NodeId<Func>) -> Self {
//         Self::Func(value)
//     }
// }

// impl From<NodeId<Call>> for Expr {
//     fn from(value: NodeId<Call>) -> Self {
//         Self::Call(value)
//     }
// }
