//! Generic substitution machinery shared across the compiler.
//!
//! This crate is intentionally dependency-free. It defines the [`Substitutable`]
//! trait, generic in the substitution type `S`, along with implementations for
//! standard-library types and the [`merge`] family of helpers used to combine
//! the `Option` results of `try_apply`.
//!
//! Implementations for crate-local collection types live in their respective
//! crates (`kola-collections`, `kola-tree`, ...), and domain-specific
//! implementations (e.g. for symbols or monotypes) live in the crates that own
//! those types.

#![feature(never_type)]

use indexmap::IndexMap;
use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap, VecDeque},
    hash::Hash,
};

/// A value to which a substitution of type `S` can be applied.
///
/// `try_apply` returns `None` when nothing changed, which allows callers to
/// avoid cloning when a substitution is a no-op.
pub trait Substitutable<S>: Sized {
    /// Apply a substitution, returning `None` if there was nothing to apply.
    fn try_apply(&self, s: &mut S) -> Option<Self>;

    /// Apply a substitution, returning `self` unchanged if nothing applied.
    fn apply(self, s: &mut S) -> Self {
        self.try_apply(s).unwrap_or(self)
    }

    /// Apply a substitution in place.
    fn apply_mut(&mut self, s: &mut S) {
        if let Some(new) = self.try_apply(s) {
            *self = new;
        }
    }

    /// Apply a substitution, borrowing when nothing changed.
    fn apply_cow(&self, s: &mut S) -> Cow<'_, Self>
    where
        Self: Clone,
    {
        match self.try_apply(s) {
            Some(t) => Cow::Owned(t),
            None => Cow::Borrowed(self),
        }
    }
}

impl<S> Substitutable<S> for ! {
    fn try_apply(&self, _s: &mut S) -> Option<Self> {
        None
    }
}

impl<S, T, U> Substitutable<S> for (T, U)
where
    T: Substitutable<S> + Clone,
    U: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result = None;

        if let Some(t) = self.0.try_apply(s) {
            result.get_or_insert_with(|| self.clone()).0 = t;
        }

        if let Some(u) = self.1.try_apply(s) {
            result.get_or_insert_with(|| self.clone()).1 = u;
        }

        result
    }
}

impl<S, T> Substitutable<S> for Option<T>
where
    T: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        match self {
            Some(t) => t.try_apply(s).map(Some),
            None => None,
        }
    }

    fn apply_mut(&mut self, s: &mut S) {
        if let Some(value) = self {
            value.apply_mut(s);
        }
    }
}

impl<S, T> Substitutable<S> for Vec<T>
where
    T: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result = None;

        for (i, item) in self.iter().enumerate() {
            if let Some(next) = item.try_apply(s) {
                result.get_or_insert_with(|| self.clone())[i] = next;
            }
        }

        result
    }

    fn apply_mut(&mut self, s: &mut S) {
        for item in self.iter_mut() {
            item.apply_mut(s);
        }
    }
}

impl<S, T> Substitutable<S> for VecDeque<T>
where
    T: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result = None;

        for (i, item) in self.iter().enumerate() {
            if let Some(next) = item.try_apply(s) {
                result.get_or_insert_with(|| self.clone())[i] = next;
            }
        }

        result
    }

    fn apply_mut(&mut self, s: &mut S) {
        for item in self.iter_mut() {
            item.apply_mut(s);
        }
    }
}

impl<S, K, V> Substitutable<S> for HashMap<K, V>
where
    K: Eq + Clone + Hash,
    V: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result = None;

        for (key, value) in self.iter() {
            if let Some(next) = value.try_apply(s) {
                result
                    .get_or_insert_with(|| self.clone())
                    .insert(key.clone(), next);
            }
        }

        result
    }

    fn apply_mut(&mut self, s: &mut S) {
        for value in self.values_mut() {
            value.apply_mut(s);
        }
    }
}

impl<S, K, V> Substitutable<S> for IndexMap<K, V>
where
    K: Eq + Clone + Hash,
    V: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result = None;

        for (key, value) in self.iter() {
            if let Some(next) = value.try_apply(s) {
                result
                    .get_or_insert_with(|| self.clone())
                    .insert(key.clone(), next);
            }
        }

        result
    }

    fn apply_mut(&mut self, s: &mut S) {
        for value in self.values_mut() {
            value.apply_mut(s);
        }
    }
}

impl<S, K, V> Substitutable<S> for BTreeMap<K, V>
where
    K: Ord + Clone,
    V: Substitutable<S> + Clone,
{
    fn try_apply(&self, s: &mut S) -> Option<Self> {
        let mut result = None;

        for (key, value) in self.iter() {
            if let Some(next) = value.try_apply(s) {
                result
                    .get_or_insert_with(|| self.clone())
                    .insert(key.clone(), next);
            }
        }

        result
    }
}

/// Combine the results of two `try_apply` calls, filling in defaults for the
/// unchanged side.
pub fn merge<A, B, DA, DB>(
    a: Option<A>,
    default_a: DA,
    b: Option<B>,
    default_b: DB,
) -> Option<(A, B)>
where
    DA: FnOnce() -> A,
    DB: FnOnce() -> B,
{
    match (a, b) {
        (Some(a), Some(b)) => Some((a, b)),
        (Some(a), None) => Some((a, default_b())),
        (None, Some(b)) => Some((default_a(), b)),
        (None, None) => None,
    }
}

/// Combine the results of three `try_apply` calls.
pub fn merge3<A, B, C, DA, DB, DC>(
    a: Option<A>,
    default_a: DA,
    b: Option<B>,
    default_b: DB,
    c: Option<C>,
    default_c: DC,
) -> Option<(A, B, C)>
where
    DA: Fn() -> A,
    DB: Fn() -> B,
    DC: Fn() -> C,
{
    merge(a, &default_a, b, &default_b).and_then(|(a, b)| {
        merge(Some((a, b)), || (default_a(), default_b()), c, default_c)
            .map(|((a, b), c)| (a, b, c))
    })
}

/// Combine the results of four `try_apply` calls.
pub fn merge4<A, B, C, D, DA, DB, DC, DD>(
    a: Option<A>,
    default_a: DA,
    b: Option<B>,
    default_b: DB,
    c: Option<C>,
    default_c: DC,
    d: Option<D>,
    default_d: DD,
) -> Option<(A, B, C, D)>
where
    DA: Fn() -> A,
    DB: Fn() -> B,
    DC: Fn() -> C,
    DD: Fn() -> D,
{
    merge3(a, &default_a, b, &default_b, c, &default_c).and_then(|(a, b, c)| {
        merge(
            Some((a, b, c)),
            || (default_a(), default_b(), default_c()),
            d,
            default_d,
        )
        .map(|((a, b, c), d)| (a, b, c, d))
    })
}

/// Combine the results of five `try_apply` calls.
pub fn merge5<A, B, C, D, E, DA, DB, DC, DD, DE>(
    a: Option<A>,
    default_a: DA,
    b: Option<B>,
    default_b: DB,
    c: Option<C>,
    default_c: DC,
    d: Option<D>,
    default_d: DD,
    e: Option<E>,
    default_e: DE,
) -> Option<(A, B, C, D, E)>
where
    DA: Fn() -> A,
    DB: Fn() -> B,
    DC: Fn() -> C,
    DD: Fn() -> D,
    DE: Fn() -> E,
{
    merge4(a, &default_a, b, &default_b, c, &default_c, d, &default_d).and_then(|(a, b, c, d)| {
        merge(
            Some((a, b, c, d)),
            || (default_a(), default_b(), default_c(), default_d()),
            e,
            default_e,
        )
        .map(|((a, b, c, d), e)| (a, b, c, d, e))
    })
}

/// Combine the results of six `try_apply` calls.
pub fn merge6<A, B, C, D, E, F, DA, DB, DC, DD, DE, DF>(
    a: Option<A>,
    default_a: DA,
    b: Option<B>,
    default_b: DB,
    c: Option<C>,
    default_c: DC,
    d: Option<D>,
    default_d: DD,
    e: Option<E>,
    default_e: DE,
    f: Option<F>,
    default_f: DF,
) -> Option<(A, B, C, D, E, F)>
where
    DA: Fn() -> A,
    DB: Fn() -> B,
    DC: Fn() -> C,
    DD: Fn() -> D,
    DE: Fn() -> E,
    DF: Fn() -> F,
{
    merge5(
        a, &default_a, b, &default_b, c, &default_c, d, &default_d, e, &default_e,
    )
    .and_then(|(a, b, c, d, e)| {
        merge(
            Some((a, b, c, d, e)),
            || {
                (
                    default_a(),
                    default_b(),
                    default_c(),
                    default_d(),
                    default_e(),
                )
            },
            f,
            default_f,
        )
        .map(|((a, b, c, d, e), f)| (a, b, c, d, e, f))
    })
}
