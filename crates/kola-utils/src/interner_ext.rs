use std::{
    borrow::Borrow,
    fmt,
    hash::{Hash, RandomState},
};

use camino::Utf8Path;
use serde::Serialize;

use crate::{display::DisplayWith, interner::Interner, serde::SerializeWith};

pub struct WithInterner<'a, T, I> {
    pub value: &'a T,
    pub interner: &'a I,
}

impl<'a, T, I> WithInterner<'a, T, I> {
    pub fn new(value: &'a T, interner: &'a I) -> Self {
        Self { value, interner }
    }
}

impl<T, I> fmt::Display for WithInterner<'_, T, I>
where
    T: DisplayWith<I>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f, self.interner)
    }
}

impl<T, I> Serialize for WithInterner<'_, T, I>
where
    T: SerializeWith<I>,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer, self.interner)
    }
}

pub trait InternerExt<B, S>: Sized
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    fn with<'a, T>(&'a self, value: &'a T) -> WithInterner<'a, T, Interner<B, S>>;

    #[inline]
    fn to_string<T>(&self, value: &T) -> String
    where
        T: DisplayWith<Interner<B, S>>,
    {
        self.with(value).to_string()
    }

    #[inline]
    fn to_json<T>(&self, value: &T) -> serde_json::Result<String>
    where
        T: SerializeWith<Interner<B, S>>,
    {
        serde_json::to_string_pretty(&self.with(value))
    }
}

impl<S> InternerExt<str, S> for Interner<str, S> {
    fn with<'a, T>(&'a self, value: &'a T) -> WithInterner<'a, T, Interner<str, S>> {
        WithInterner::new(value, self)
    }
}

impl<S> InternerExt<Utf8Path, S> for Interner<Utf8Path, S> {
    fn with<'a, T>(&'a self, value: &'a T) -> WithInterner<'a, T, Interner<Utf8Path, S>> {
        WithInterner::new(value, self)
    }
}
