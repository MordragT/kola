use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{
    Binary, Call, Case, Func, Ident, If, InnerNode, Let, List, Literal, Node, Record, RecordExtend,
    RecordRestrict, RecordSelect, RecordUpdate, Unary,
};
use crate::{
    Phase,
    id::NodeId,
    meta::{Attached, Meta},
    print::TreePrinter,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExprError;

impl Printable<TreePrinter> for ExprError {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        "ExprError".red().display_in(arena)
    }
}

impl<P: Phase> Attached<P> for ExprError {
    type Meta = P::ExprError;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::ExprError(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::ExprError(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::ExprError(m) => Some(m),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
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

impl InnerNode for Expr {
    fn to_inner_ref(node: &Node) -> Option<&Self> {
        match node {
            Node::Expr(e) => Some(e),
            _ => None,
        }
    }

    fn to_inner_mut(node: &mut Node) -> Option<&mut Self> {
        match node {
            Node::Expr(e) => Some(e),
            _ => None,
        }
    }
}

impl<P: Phase> Attached<P> for Expr {
    type Meta = P::Expr;

    fn into_meta(attached: Self::Meta) -> Meta<P> {
        Meta::Expr(attached)
    }

    fn to_attached_ref(meta: &Meta<P>) -> Option<&Self::Meta> {
        match meta {
            Meta::Expr(m) => Some(m),
            _ => None,
        }
    }

    fn to_attached_mut(meta: &mut Meta<P>) -> Option<&mut Self::Meta> {
        match meta {
            Meta::Expr(m) => Some(m),
            _ => None,
        }
    }
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

impl TryFrom<Node> for Expr {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, ()> {
        match value {
            Node::Expr(e) => Ok(e),
            _ => Err(()),
        }
    }
}

// TODO Function Call

impl Expr {
    pub fn as_error(&self) -> Option<&ExprError> {
        match self {
            Self::Error(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&NodeId<Literal>> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_ident(&self) -> Option<&NodeId<Ident>> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_list(&self) -> Option<&NodeId<List>> {
        match self {
            Self::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_record(&self) -> Option<&NodeId<Record>> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn as_record_select(&self) -> Option<&NodeId<RecordSelect>> {
        match self {
            Self::RecordSelect(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_record_extend(&self) -> Option<&NodeId<RecordExtend>> {
        match self {
            Self::RecordExtend(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_record_update(&self) -> Option<&NodeId<RecordUpdate>> {
        match self {
            Self::RecordUpdate(u) => Some(u),
            _ => None,
        }
    }

    pub fn as_unary(&self) -> Option<&NodeId<Unary>> {
        match self {
            Self::Unary(u) => Some(u),
            _ => None,
        }
    }

    pub fn as_binary(&self) -> Option<&NodeId<Binary>> {
        match self {
            Self::Binary(b) => Some(b),
            _ => None,
        }
    }

    pub fn as_let(&self) -> Option<&NodeId<Let>> {
        match self {
            Self::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_if(&self) -> Option<&NodeId<If>> {
        match self {
            Self::If(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_case(&self) -> Option<&NodeId<Case>> {
        match self {
            Self::Case(c) => Some(c),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<&NodeId<Func>> {
        match self {
            Self::Func(f) => Some(f),
            _ => None,
        }
    }

    pub fn as_call(&self) -> Option<&NodeId<Call>> {
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

    pub fn into_literal(self) -> Option<NodeId<Literal>> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<NodeId<Ident>> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_list(self) -> Option<NodeId<List>> {
        match self {
            Self::List(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<NodeId<Record>> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn into_record_select(self) -> Option<NodeId<RecordSelect>> {
        match self {
            Self::RecordSelect(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_record_extend(self) -> Option<NodeId<RecordExtend>> {
        match self {
            Self::RecordExtend(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_record_update(self) -> Option<NodeId<RecordUpdate>> {
        match self {
            Self::RecordUpdate(u) => Some(u),
            _ => None,
        }
    }

    pub fn into_unary(self) -> Option<NodeId<Unary>> {
        match self {
            Self::Unary(u) => Some(u),
            _ => None,
        }
    }

    pub fn into_binary(self) -> Option<NodeId<Binary>> {
        match self {
            Self::Binary(b) => Some(b),
            _ => None,
        }
    }

    pub fn into_let(self) -> Option<NodeId<Let>> {
        match self {
            Self::Let(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_if(self) -> Option<NodeId<If>> {
        match self {
            Self::If(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_case(self) -> Option<NodeId<Case>> {
        match self {
            Self::Case(c) => Some(c),
            _ => None,
        }
    }

    pub fn into_func(self) -> Option<NodeId<Func>> {
        match self {
            Self::Func(f) => Some(f),
            _ => None,
        }
    }

    pub fn into_call(self) -> Option<NodeId<Call>> {
        match self {
            Self::Call(c) => Some(c),
            _ => None,
        }
    }
}

impl From<NodeId<Ident>> for Expr {
    fn from(value: NodeId<Ident>) -> Self {
        Self::Ident(value)
    }
}

impl From<NodeId<Literal>> for Expr {
    fn from(value: NodeId<Literal>) -> Self {
        Self::Literal(value)
    }
}

impl From<NodeId<List>> for Expr {
    fn from(value: NodeId<List>) -> Self {
        Self::List(value)
    }
}

impl From<NodeId<Record>> for Expr {
    fn from(value: NodeId<Record>) -> Self {
        Self::Record(value)
    }
}

impl From<NodeId<RecordSelect>> for Expr {
    fn from(value: NodeId<RecordSelect>) -> Self {
        Self::RecordSelect(value)
    }
}

impl From<NodeId<RecordExtend>> for Expr {
    fn from(value: NodeId<RecordExtend>) -> Self {
        Self::RecordExtend(value)
    }
}

impl From<NodeId<RecordRestrict>> for Expr {
    fn from(value: NodeId<RecordRestrict>) -> Self {
        Self::RecordRestrict(value)
    }
}

impl From<NodeId<RecordUpdate>> for Expr {
    fn from(value: NodeId<RecordUpdate>) -> Self {
        Self::RecordUpdate(value)
    }
}

impl From<NodeId<Unary>> for Expr {
    fn from(value: NodeId<Unary>) -> Self {
        Self::Unary(value)
    }
}

impl From<NodeId<Binary>> for Expr {
    fn from(value: NodeId<Binary>) -> Self {
        Self::Binary(value)
    }
}

impl From<NodeId<Let>> for Expr {
    fn from(value: NodeId<Let>) -> Self {
        Self::Let(value)
    }
}

impl From<NodeId<If>> for Expr {
    fn from(value: NodeId<If>) -> Self {
        Self::If(value)
    }
}

impl From<NodeId<Case>> for Expr {
    fn from(value: NodeId<Case>) -> Self {
        Self::Case(value)
    }
}

impl From<NodeId<Func>> for Expr {
    fn from(value: NodeId<Func>) -> Self {
        Self::Func(value)
    }
}

impl From<NodeId<Call>> for Expr {
    fn from(value: NodeId<Call>) -> Self {
        Self::Call(value)
    }
}
