use std::{borrow::Cow, collections::HashMap};

use super::types::{MonoType, TypeVar};

pub type Substitution = HashMap<TypeVar, MonoType>;

/// Path compression is a technique commonly used in Union-Find data structures.
/// We apply it here so that whenever a chain of substitutions is traversed,
/// each variable is updated to point to its ultimate value. For example, the
/// chain:
///
/// `t0 ↦ t1`, `t1 ↦ t2`, and `t2 ↦ int`
///
/// becomes
///
/// `t0 ↦ int`, `t1 ↦ int`, and `t2 ↦ int`
///
/// Rather than updating the actual mappings,
/// this cache maintains these compressed mappings.
pub type Cache = HashMap<TypeVar, MonoType>;

/// A type is `Substitutable` if a substitution can be applied to it.
pub trait Substitutable: Sized {
    /// Apply a substitution to a type variable.
    fn apply(self, s: &mut Substitution, cache: &mut Cache) -> Self {
        self.try_apply(s, cache).unwrap_or(self)
    }

    /// Apply a substitution to a type variable.
    fn apply_mut(&mut self, s: &mut Substitution, cache: &mut Cache) {
        if let Some(new) = self.try_apply(s, cache) {
            *self = new;
        }
    }

    /// Apply a substitution to a type variable.
    fn apply_cow(&self, s: &mut Substitution, cache: &mut Cache) -> Cow<'_, Self>
    where
        Self: Clone,
    {
        match self.try_apply(s, cache) {
            Some(t) => Cow::Owned(t),
            None => Cow::Borrowed(self),
        }
    }

    /// Apply a non-mutating substitution to a type variable.
    /// Should return `None` if there was nothing to apply
    /// which allows for optimizations.
    fn try_apply(&self, s: &mut Substitution, cache: &mut Cache) -> Option<Self>;
}

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
