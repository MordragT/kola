use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Node<T, M> {
    pub inner: T, // TODO Box
    pub meta: M,
}

impl<T, M> Node<T, M> {
    pub fn new(inner: T, meta: M) -> Self {
        Self { inner, meta }
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    pub fn into_inner(self) -> T {
        self.inner
    }

    pub fn map<F, U>(self, f: F) -> Node<T, U>
    where
        F: FnOnce(M) -> U,
    {
        let Node { inner, meta } = self;
        let meta = f(meta);
        Node { inner, meta }
    }
}

impl<T, M> Deref for Node<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T, M> DerefMut for Node<T, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
