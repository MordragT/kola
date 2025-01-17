use std::{
    fmt,
    ops::{Deref, DerefMut},
};

use serde::{Deserialize, Serialize};

use crate::{semantic::types::MonoType, syntax::Span};

use super::print::{JoinIn, NotateIn, Printable};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Node<T> {
    inner: Box<T>,
    pub span: Span,
    pub ty: MonoType,
}

impl<T> Node<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self {
            inner: Box::new(inner),
            span,
            ty: MonoType::variable(),
        }
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub fn into_inner(self) -> T {
        *self.inner
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn ty(&self) -> &MonoType {
        &self.ty
    }

    pub fn ty_mut(&mut self) -> &mut MonoType {
        &mut self.ty
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T> PartialOrd for Node<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl<T> Ord for Node<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<T> Printable for Node<T>
where
    T: Printable,
{
    fn notate<'a>(&'a self, arena: &'a super::print::Arena<'a>) -> super::print::Notation<'a> {
        let Self { inner, span, ty } = self;

        let head = span.notate_in(arena).then(arena.notate(":"), arena);

        let inner = inner.notate(arena);
        let ty = ty.notate_in(arena);

        let single = [
            arena.notate(" "),
            inner.clone().flatten(arena),
            arena.notate(": "),
            ty.clone().flatten(arena),
        ]
        .join_in(arena);

        let multi = [
            arena.break_line(),
            inner,
            arena.break_line(),
            arena.notate(":"),
            ty,
        ]
        .join_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}
