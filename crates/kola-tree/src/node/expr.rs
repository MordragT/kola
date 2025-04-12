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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ExprError;

impl Printable<TreePrinter> for ExprError {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        "ExprError".red().display_in(arena)
    }
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Expr {
    Error(NodeId<ExprError>),
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
            Self::Error(e) => e.get(&with.tree).notate(with, arena),
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

impl Expr {
    #[inline]
    pub fn to_error(self) -> Option<NodeId<ExprError>> {
        as_variant!(self, Self::Error)
    }

    #[inline]
    pub fn to_literal(self) -> Option<NodeId<Literal>> {
        as_variant!(self, Self::Literal)
    }

    #[inline]
    pub fn to_ident(self) -> Option<NodeId<Ident>> {
        as_variant!(self, Self::Ident)
    }

    #[inline]
    pub fn to_list(self) -> Option<NodeId<List>> {
        as_variant!(self, Self::List)
    }

    #[inline]
    pub fn to_record(self) -> Option<NodeId<Record>> {
        as_variant!(self, Self::Record)
    }

    #[inline]
    pub fn to_record_select(self) -> Option<NodeId<RecordSelect>> {
        as_variant!(self, Self::RecordSelect)
    }

    #[inline]
    pub fn to_record_extend(self) -> Option<NodeId<RecordExtend>> {
        as_variant!(self, Self::RecordExtend)
    }

    #[inline]
    pub fn to_record_update(self) -> Option<NodeId<RecordUpdate>> {
        as_variant!(self, Self::RecordUpdate)
    }

    #[inline]
    pub fn to_unary(self) -> Option<NodeId<Unary>> {
        as_variant!(self, Self::Unary)
    }

    #[inline]
    pub fn to_binary(self) -> Option<NodeId<Binary>> {
        as_variant!(self, Self::Binary)
    }

    #[inline]
    pub fn to_let(self) -> Option<NodeId<Let>> {
        as_variant!(self, Self::Let)
    }

    #[inline]
    pub fn to_if(self) -> Option<NodeId<If>> {
        as_variant!(self, Self::If)
    }

    #[inline]
    pub fn to_case(self) -> Option<NodeId<Case>> {
        as_variant!(self, Self::Case)
    }

    #[inline]
    pub fn to_func(self) -> Option<NodeId<Func>> {
        as_variant!(self, Self::Func)
    }

    #[inline]
    pub fn to_call(self) -> Option<NodeId<Call>> {
        as_variant!(self, Self::Call)
    }

    #[inline]
    pub fn is_error(self) -> bool {
        matches!(self, Self::Error(_))
    }

    #[inline]
    pub fn is_literal(self) -> bool {
        matches!(self, Self::Literal(_))
    }

    #[inline]
    pub fn is_ident(self) -> bool {
        matches!(self, Self::Ident(_))
    }

    #[inline]
    pub fn is_list(self) -> bool {
        matches!(self, Self::List(_))
    }

    #[inline]
    pub fn is_record(self) -> bool {
        matches!(self, Self::Record(_))
    }

    #[inline]
    pub fn is_record_select(self) -> bool {
        matches!(self, Self::RecordSelect(_))
    }

    #[inline]
    pub fn is_record_extend(self) -> bool {
        matches!(self, Self::RecordExtend(_))
    }

    #[inline]
    pub fn is_record_update(self) -> bool {
        matches!(self, Self::RecordUpdate(_))
    }

    #[inline]
    pub fn is_unary(self) -> bool {
        matches!(self, Self::Unary(_))
    }

    #[inline]
    pub fn is_binary(self) -> bool {
        matches!(self, Self::Binary(_))
    }

    #[inline]
    pub fn is_let(self) -> bool {
        matches!(self, Self::Let(_))
    }

    #[inline]
    pub fn is_if(self) -> bool {
        matches!(self, Self::If(_))
    }

    #[inline]
    pub fn is_case(self) -> bool {
        matches!(self, Self::Case(_))
    }

    #[inline]
    pub fn is_func(self) -> bool {
        matches!(self, Self::Func(_))
    }

    #[inline]
    pub fn is_call(self) -> bool {
        matches!(self, Self::Call(_))
    }
}
