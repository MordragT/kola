use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

use crate::syntax::{print::prelude::*, Span};

use super::{Metadata, Tree, TryAsMut, TryAsRef};

#[derive(Clone, Debug, PartialEq, Default)]
pub struct NodePool {
    nodes: Vec<Node>,
}

impl NodePool {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get<T>(&self, id: NodeId<T>) -> &T
    where
        Node: TryAsRef<T>,
    {
        let node = &self.nodes[id.as_usize()];
        node.try_as_ref().unwrap()
    }

    pub fn get_mut<T>(&mut self, id: NodeId<T>) -> &mut T
    where
        Node: TryAsMut<T>,
    {
        let node = &mut self.nodes[id.as_usize()];
        node.try_as_mut().unwrap()
    }

    pub fn insert<T>(&mut self, node: T) -> NodeId<T>
    where
        T: Into<Node>,
    {
        let id = self.nodes.len();
        self.nodes.push(node.into());
        NodeId::new(id as u32)
    }

    pub fn root(&self) -> NodeId<super::Expr> {
        let id = self.nodes.len() - 1;
        NodeId::new(id as u32)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Node> {
        self.nodes.iter()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct NodeId<T> {
    id: u32,
    t: PhantomData<T>,
}

impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            t: PhantomData,
        }
    }
}

impl<T> Copy for NodeId<T> {}

impl<T> NodeId<T> {
    pub(super) fn new(id: u32) -> Self {
        Self { id, t: PhantomData }
    }

    pub fn as_usize(&self) -> usize {
        self.id as usize
    }

    // pub fn with_span<M>(self, tree: &Tree<M>) -> (&T, Span)
    // where
    //     Node: TryAsRef<T>,
    //     M: Metadata,
    // {
    //     let t = tree.nodes.get(self);
    //     let span = self.span(&tree.meta);

    //     (t, span)
    // }

    pub fn get(self, pool: &NodePool) -> &T
    where
        Node: TryAsRef<T>,
    {
        pool.get(self)
    }

    pub fn get_mut(self, pool: &mut NodePool) -> &mut T
    where
        Node: TryAsMut<T>,
    {
        pool.get_mut(self)
    }

    pub fn meta<M>(self, pool: &Vec<M>) -> &M {
        &pool[self.as_usize()]
    }

    pub fn meta_mut<M>(self, pool: &mut Vec<M>) -> &mut M {
        &mut pool[self.as_usize()]
    }

    pub fn span<M>(self, pool: &Vec<M>) -> Span
    where
        M: Metadata,
    {
        pool[self.as_usize()].span()
    }

    // TODO this wouldnt work with the getter logic for Tree
    // pub fn cast<U>(self) -> NodeId<U>
    // where
    //     U: From<T>,
    // {
    //     NodeId {
    //         id: self.id,
    //         t: PhantomData,
    //     }
    // }
}

impl<T, M> Printable<Tree<M>> for NodeId<T>
where
    M: Metadata,
    T: Printable<Tree<M>>,
    Node: TryAsRef<T>,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        let meta = self.meta(&with.meta);

        let span = meta.span().display_in(arena);
        let node = self.get(&with.nodes).notate(with, arena);
        let ty = meta
            .ty()
            .map(|ty| arena.notate(": ").then(ty.display_in(arena), arena));

        let single = [
            arena.just(' '),
            node.clone().flatten(arena),
            ty.clone()
                .map(|ty| arena.just(' ').then(ty, arena))
                .or_not(arena)
                .flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            node,
            ty.map(|ty| arena.newline().then(ty, arena)).or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        span.then(single.or(multi, arena), arena)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Name(super::Name),
    Ident(super::Ident),
    Literal(super::Literal),
    List(super::List),
    Property(super::Property),
    Record(super::Record),
    RecordSelect(super::RecordSelect),
    RecordExtend(super::RecordExtend),
    RecordRestrict(super::RecordRestrict),
    RecordUpdate(super::RecordUpdate),
    UnaryOp(super::UnaryOp),
    Unary(super::Unary),
    BinaryOp(super::BinaryOp),
    Binary(super::Binary),
    Let(super::Let),
    PatError(super::PatError),
    Wildcard(super::Wildcard),
    LiteralPat(super::LiteralPat),
    IdentPat(super::IdentPat),
    PropertyPat(super::PropertyPat),
    RecordPat(super::RecordPat),
    Pat(super::Pat),
    Branch(super::Branch),
    Case(super::Case),
    If(super::If),
    Func(super::Func),
    Call(super::Call),
    ExprError(super::ExprError),
    Expr(super::Expr),
}

impl From<super::Name> for Node {
    fn from(value: super::Name) -> Self {
        Self::Name(value)
    }
}

impl From<super::Ident> for Node {
    fn from(value: super::Ident) -> Self {
        Self::Ident(value)
    }
}

impl From<super::Literal> for Node {
    fn from(value: super::Literal) -> Self {
        Self::Literal(value)
    }
}

impl From<super::List> for Node {
    fn from(value: super::List) -> Self {
        Self::List(value)
    }
}

impl From<super::Property> for Node {
    fn from(value: super::Property) -> Self {
        Self::Property(value)
    }
}

impl From<super::Record> for Node {
    fn from(value: super::Record) -> Self {
        Self::Record(value)
    }
}

impl From<super::RecordSelect> for Node {
    fn from(value: super::RecordSelect) -> Self {
        Self::RecordSelect(value)
    }
}

impl From<super::RecordExtend> for Node {
    fn from(value: super::RecordExtend) -> Self {
        Self::RecordExtend(value)
    }
}

impl From<super::RecordRestrict> for Node {
    fn from(value: super::RecordRestrict) -> Self {
        Self::RecordRestrict(value)
    }
}

impl From<super::RecordUpdate> for Node {
    fn from(value: super::RecordUpdate) -> Self {
        Self::RecordUpdate(value)
    }
}

impl From<super::UnaryOp> for Node {
    fn from(value: super::UnaryOp) -> Self {
        Self::UnaryOp(value)
    }
}

impl From<super::Unary> for Node {
    fn from(value: super::Unary) -> Self {
        Self::Unary(value)
    }
}

impl From<super::BinaryOp> for Node {
    fn from(value: super::BinaryOp) -> Self {
        Self::BinaryOp(value)
    }
}

impl From<super::Binary> for Node {
    fn from(value: super::Binary) -> Self {
        Self::Binary(value)
    }
}

impl From<super::Let> for Node {
    fn from(value: super::Let) -> Self {
        Self::Let(value)
    }
}

impl From<super::PatError> for Node {
    fn from(value: super::PatError) -> Self {
        Self::PatError(value)
    }
}

impl From<super::Wildcard> for Node {
    fn from(value: super::Wildcard) -> Self {
        Self::Wildcard(value)
    }
}

impl From<super::LiteralPat> for Node {
    fn from(value: super::LiteralPat) -> Self {
        Self::LiteralPat(value)
    }
}

impl From<super::IdentPat> for Node {
    fn from(value: super::IdentPat) -> Self {
        Self::IdentPat(value)
    }
}

impl From<super::PropertyPat> for Node {
    fn from(value: super::PropertyPat) -> Self {
        Self::PropertyPat(value)
    }
}

impl From<super::RecordPat> for Node {
    fn from(value: super::RecordPat) -> Self {
        Self::RecordPat(value)
    }
}

impl From<super::Pat> for Node {
    fn from(value: super::Pat) -> Self {
        Self::Pat(value)
    }
}

impl From<super::Branch> for Node {
    fn from(value: super::Branch) -> Self {
        Self::Branch(value)
    }
}

impl From<super::Case> for Node {
    fn from(value: super::Case) -> Self {
        Self::Case(value)
    }
}

impl From<super::If> for Node {
    fn from(value: super::If) -> Self {
        Self::If(value)
    }
}

impl From<super::Func> for Node {
    fn from(value: super::Func) -> Self {
        Self::Func(value)
    }
}

impl From<super::Call> for Node {
    fn from(value: super::Call) -> Self {
        Self::Call(value)
    }
}

impl From<super::ExprError> for Node {
    fn from(value: super::ExprError) -> Self {
        Self::ExprError(value)
    }
}

impl From<super::Expr> for Node {
    fn from(value: super::Expr) -> Self {
        Self::Expr(value)
    }
}

impl TryAsRef<super::Name> for Node {
    fn try_as_ref(&self) -> Option<&super::Name> {
        match self {
            Self::Name(n) => Some(n),
            _ => None,
        }
    }
}

impl TryAsRef<super::Ident> for Node {
    fn try_as_ref(&self) -> Option<&super::Ident> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }
}

impl TryAsRef<super::Literal> for Node {
    fn try_as_ref(&self) -> Option<&super::Literal> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }
}

impl TryAsRef<super::List> for Node {
    fn try_as_ref(&self) -> Option<&super::List> {
        match self {
            Self::List(l) => Some(l),
            _ => None,
        }
    }
}

impl TryAsRef<super::Property> for Node {
    fn try_as_ref(&self) -> Option<&super::Property> {
        match self {
            Self::Property(p) => Some(p),
            _ => None,
        }
    }
}

impl TryAsRef<super::Record> for Node {
    fn try_as_ref(&self) -> Option<&super::Record> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsRef<super::RecordSelect> for Node {
    fn try_as_ref(&self) -> Option<&super::RecordSelect> {
        match self {
            Self::RecordSelect(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsRef<super::RecordExtend> for Node {
    fn try_as_ref(&self) -> Option<&super::RecordExtend> {
        match self {
            Self::RecordExtend(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsRef<super::RecordRestrict> for Node {
    fn try_as_ref(&self) -> Option<&super::RecordRestrict> {
        match self {
            Self::RecordRestrict(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsRef<super::RecordUpdate> for Node {
    fn try_as_ref(&self) -> Option<&super::RecordUpdate> {
        match self {
            Self::RecordUpdate(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsRef<super::UnaryOp> for Node {
    fn try_as_ref(&self) -> Option<&super::UnaryOp> {
        match self {
            Self::UnaryOp(u) => Some(u),
            _ => None,
        }
    }
}

impl TryAsRef<super::Unary> for Node {
    fn try_as_ref(&self) -> Option<&super::Unary> {
        match self {
            Self::Unary(u) => Some(u),
            _ => None,
        }
    }
}

impl TryAsRef<super::BinaryOp> for Node {
    fn try_as_ref(&self) -> Option<&super::BinaryOp> {
        match self {
            Self::BinaryOp(b) => Some(b),
            _ => None,
        }
    }
}

impl TryAsRef<super::Binary> for Node {
    fn try_as_ref(&self) -> Option<&super::Binary> {
        match self {
            Self::Binary(b) => Some(b),
            _ => None,
        }
    }
}

impl TryAsRef<super::Let> for Node {
    fn try_as_ref(&self) -> Option<&super::Let> {
        match self {
            Self::Let(l) => Some(l),
            _ => None,
        }
    }
}

impl TryAsRef<super::PatError> for Node {
    fn try_as_ref(&self) -> Option<&super::PatError> {
        match self {
            Self::PatError(p) => Some(p),
            _ => None,
        }
    }
}

impl TryAsRef<super::Wildcard> for Node {
    fn try_as_ref(&self) -> Option<&super::Wildcard> {
        match self {
            Self::Wildcard(w) => Some(w),
            _ => None,
        }
    }
}

impl TryAsRef<super::LiteralPat> for Node {
    fn try_as_ref(&self) -> Option<&super::LiteralPat> {
        match self {
            Self::LiteralPat(l) => Some(l),
            _ => None,
        }
    }
}

impl TryAsRef<super::IdentPat> for Node {
    fn try_as_ref(&self) -> Option<&super::IdentPat> {
        match self {
            Self::IdentPat(i) => Some(i),
            _ => None,
        }
    }
}

impl TryAsRef<super::PropertyPat> for Node {
    fn try_as_ref(&self) -> Option<&super::PropertyPat> {
        match self {
            Self::PropertyPat(p) => Some(p),
            _ => None,
        }
    }
}

impl TryAsRef<super::RecordPat> for Node {
    fn try_as_ref(&self) -> Option<&super::RecordPat> {
        match self {
            Self::RecordPat(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsRef<super::Pat> for Node {
    fn try_as_ref(&self) -> Option<&super::Pat> {
        match self {
            Self::Pat(p) => Some(p),
            _ => None,
        }
    }
}

impl TryAsRef<super::Branch> for Node {
    fn try_as_ref(&self) -> Option<&super::Branch> {
        match self {
            Self::Branch(b) => Some(b),
            _ => None,
        }
    }
}

impl TryAsRef<super::Case> for Node {
    fn try_as_ref(&self) -> Option<&super::Case> {
        match self {
            Self::Case(c) => Some(c),
            _ => None,
        }
    }
}

impl TryAsRef<super::If> for Node {
    fn try_as_ref(&self) -> Option<&super::If> {
        match self {
            Self::If(i) => Some(i),
            _ => None,
        }
    }
}

impl TryAsRef<super::Func> for Node {
    fn try_as_ref(&self) -> Option<&super::Func> {
        match self {
            Self::Func(f) => Some(f),
            _ => None,
        }
    }
}

impl TryAsRef<super::Call> for Node {
    fn try_as_ref(&self) -> Option<&super::Call> {
        match self {
            Self::Call(c) => Some(c),
            _ => None,
        }
    }
}

impl TryAsRef<super::ExprError> for Node {
    fn try_as_ref(&self) -> Option<&super::ExprError> {
        match self {
            Self::ExprError(e) => Some(e),
            _ => None,
        }
    }
}

impl TryAsRef<super::Expr> for Node {
    fn try_as_ref(&self) -> Option<&super::Expr> {
        match self {
            Self::Expr(e) => Some(e),
            _ => None,
        }
    }
}

impl TryAsMut<super::Name> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Name> {
        match self {
            Self::Name(n) => Some(n),
            _ => None,
        }
    }
}

impl TryAsMut<super::Ident> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Ident> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }
}

impl TryAsMut<super::Literal> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Literal> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }
}

impl TryAsMut<super::List> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::List> {
        match self {
            Self::List(l) => Some(l),
            _ => None,
        }
    }
}

impl TryAsMut<super::Property> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Property> {
        match self {
            Self::Property(p) => Some(p),
            _ => None,
        }
    }
}

impl TryAsMut<super::Record> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Record> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsMut<super::RecordSelect> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::RecordSelect> {
        match self {
            Self::RecordSelect(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsMut<super::RecordExtend> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::RecordExtend> {
        match self {
            Self::RecordExtend(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsMut<super::RecordRestrict> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::RecordRestrict> {
        match self {
            Self::RecordRestrict(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsMut<super::RecordUpdate> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::RecordUpdate> {
        match self {
            Self::RecordUpdate(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsMut<super::UnaryOp> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::UnaryOp> {
        match self {
            Self::UnaryOp(u) => Some(u),
            _ => None,
        }
    }
}

impl TryAsMut<super::Unary> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Unary> {
        match self {
            Self::Unary(u) => Some(u),
            _ => None,
        }
    }
}

impl TryAsMut<super::BinaryOp> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::BinaryOp> {
        match self {
            Self::BinaryOp(b) => Some(b),
            _ => None,
        }
    }
}

impl TryAsMut<super::Binary> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Binary> {
        match self {
            Self::Binary(b) => Some(b),
            _ => None,
        }
    }
}

impl TryAsMut<super::Let> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Let> {
        match self {
            Self::Let(l) => Some(l),
            _ => None,
        }
    }
}

impl TryAsMut<super::PatError> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::PatError> {
        match self {
            Self::PatError(p) => Some(p),
            _ => None,
        }
    }
}

impl TryAsMut<super::Wildcard> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Wildcard> {
        match self {
            Self::Wildcard(w) => Some(w),
            _ => None,
        }
    }
}

impl TryAsMut<super::LiteralPat> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::LiteralPat> {
        match self {
            Self::LiteralPat(l) => Some(l),
            _ => None,
        }
    }
}

impl TryAsMut<super::IdentPat> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::IdentPat> {
        match self {
            Self::IdentPat(i) => Some(i),
            _ => None,
        }
    }
}

impl TryAsMut<super::PropertyPat> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::PropertyPat> {
        match self {
            Self::PropertyPat(p) => Some(p),
            _ => None,
        }
    }
}

impl TryAsMut<super::RecordPat> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::RecordPat> {
        match self {
            Self::RecordPat(r) => Some(r),
            _ => None,
        }
    }
}

impl TryAsMut<super::Pat> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Pat> {
        match self {
            Self::Pat(p) => Some(p),
            _ => None,
        }
    }
}

impl TryAsMut<super::Branch> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Branch> {
        match self {
            Self::Branch(b) => Some(b),
            _ => None,
        }
    }
}

impl TryAsMut<super::Case> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Case> {
        match self {
            Self::Case(c) => Some(c),
            _ => None,
        }
    }
}

impl TryAsMut<super::If> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::If> {
        match self {
            Self::If(i) => Some(i),
            _ => None,
        }
    }
}

impl TryAsMut<super::Func> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Func> {
        match self {
            Self::Func(f) => Some(f),
            _ => None,
        }
    }
}

impl TryAsMut<super::Call> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Call> {
        match self {
            Self::Call(c) => Some(c),
            _ => None,
        }
    }
}

impl TryAsMut<super::ExprError> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::ExprError> {
        match self {
            Self::ExprError(e) => Some(e),
            _ => None,
        }
    }
}

impl TryAsMut<super::Expr> for Node {
    fn try_as_mut(&mut self) -> Option<&mut super::Expr> {
        match self {
            Self::Expr(e) => Some(e),
            _ => None,
        }
    }
}
