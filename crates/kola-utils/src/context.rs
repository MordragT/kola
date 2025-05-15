use std::ops::{Deref, DerefMut};

#[macro_export]
macro_rules! define_context_trait {
    ($name:ident, $($trait:ident),* $(,)?) => {
        pub trait $name: $($trait + )* {}
        impl<T> $name for T where T: $($trait + )* {}
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WithContext<T, C> {
    pub context: C,
    pub value: T,
}

impl<T, C> WithContext<T, C> {
    pub fn new(context: C, value: T) -> Self {
        Self { context, value }
    }

    pub fn map<U, F>(self, f: F) -> WithContext<U, C>
    where
        F: FnOnce(T) -> U,
    {
        WithContext {
            context: self.context,
            value: f(self.value),
        }
    }

    pub fn map_context<U, F>(self, f: F) -> WithContext<T, U>
    where
        F: FnOnce(C) -> U,
    {
        WithContext {
            context: f(self.context),
            value: self.value,
        }
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl<T, C> Deref for WithContext<T, C> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T, C> DerefMut for WithContext<T, C> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
