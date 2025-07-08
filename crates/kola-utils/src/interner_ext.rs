use std::{
    borrow::Borrow,
    fmt,
    hash::{Hash, RandomState},
};

use camino::Utf8Path;
use serde::Serialize;

use crate::interner::{Interner, PathInterner, PathKey, StrInterner, StrKey};

pub trait DisplayWithInterner<B, S = RandomState>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &Interner<B, S>) -> fmt::Result;
}

impl DisplayWithInterner<str> for StrKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        write!(f, "{}", interner[*self])
    }
}

impl DisplayWithInterner<Utf8Path> for PathKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &PathInterner) -> fmt::Result {
        write!(f, "{}", interner[*self])
    }
}

pub trait SerializeWithInterner<B, State = RandomState>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    fn serialize<S>(&self, serializer: S, interner: &Interner<B, State>) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer;
}

pub struct WithInterner<'a, T, B, S = RandomState>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    pub value: &'a T,
    pub interner: &'a Interner<B, S>,
}

impl<'a, T, B, S> WithInterner<'a, T, B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    pub fn new(value: &'a T, interner: &'a Interner<B, S>) -> Self {
        Self { value, interner }
    }
}

impl<T, B, S> fmt::Display for WithInterner<'_, T, B, S>
where
    T: DisplayWithInterner<B, S>,
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f, self.interner)
    }
}

impl<T, B, State> Serialize for WithInterner<'_, T, B, State>
where
    T: SerializeWithInterner<B, State>,
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.value.serialize(serializer, self.interner)
    }
}

pub trait InternerExt<B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    fn with<'a, T>(&'a self, value: &'a T) -> WithInterner<'a, T, B, S>;

    #[inline]
    fn to_string<T>(&self, value: &T) -> String
    where
        T: DisplayWithInterner<B, S>,
    {
        self.with(value).to_string()
    }

    #[inline]
    fn to_json<T>(&self, value: &T) -> serde_json::Result<String>
    where
        T: SerializeWithInterner<B, S>,
    {
        serde_json::to_string_pretty(&self.with(value))
    }
}

impl<S> InternerExt<str, S> for Interner<str, S> {
    fn with<'a, T>(&'a self, value: &'a T) -> WithInterner<'a, T, str, S> {
        WithInterner::new(value, self)
    }
}

impl<S> InternerExt<Utf8Path, S> for Interner<Utf8Path, S> {
    fn with<'a, T>(&'a self, value: &'a T) -> WithInterner<'a, T, Utf8Path, S> {
        WithInterner::new(value, self)
    }
}
