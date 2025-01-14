use std::ops::{Deref, DerefMut};

use crate::{semantic::types::MonoType, syntax::Span};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    // pub fn map<F, U>(self, f: F) -> Node<T, U>
    // where
    //     F: FnOnce(M) -> U,
    // {
    //     let Node { inner, meta } = self;
    //     let meta = f(meta);
    //     Node { inner, meta }
    // }
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

// impl<T, M> Substitutable for Node<T, M>
// where
//     T: Clone,
//     M: Substitutable,
// {
//     fn apply(mut self, s: &mut Substitution, cache: &mut Cache) -> Self {
//         self.meta = self.meta.apply(s, cache);
//         self
//     }

//     fn apply_mut(&mut self, s: &mut Substitution, cache: &mut Cache) {
//         self.meta.apply_mut(s, cache);
//     }

//     // TODO visit child nodes

//     fn try_apply(&self, s: &mut Substitution, cache: &mut Cache) -> Option<Self> {
//         self.meta.try_apply(s, cache).map(|meta| Self {
//             meta,
//             inner: self.inner.clone(),
//         })
//     }
// }
