use owo_colors::Style;
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};

use crate::{
    semantic::{types::MonoType, Constraints},
    syntax::Span,
};

use super::print::prelude::*;

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

    pub fn constrain(&self, with: &MonoType, cons: &mut Constraints) {
        cons.constrain(self.ty.clone(), with.clone(), self.span);
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
    fn notate<'a>(&'a self, arena: &'a Bump) -> super::print::Notation<'a> {
        let Self { inner, span, ty } = self;

        let head = span.display_in(arena);

        let inner = inner.notate(arena);
        let ty = ty.display_with_in(Style::new().green(), arena);

        let single = [
            arena.notate(" "),
            inner.clone().flatten(arena),
            arena.notate(" : "),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            inner,
            arena.newline(),
            arena.notate(": "),
            ty,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}
