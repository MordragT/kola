use std::{borrow::Cow, collections::HashMap, fmt, ops::ControlFlow};

use owo_colors::OwoColorize;

// use crate::syntax::visit::{Visitable, VisitorMut};

use super::types::{MonoType, TypeVar};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Substitution {
    table: HashMap<TypeVar, MonoType>,
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
    cache: HashMap<TypeVar, MonoType>,
}

impl fmt::Display for Substitution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (var, ty) in &self.table {
            writeln!(f, "{var}\t{}", ty.green())?;
        }

        Ok(())
    }
}

impl Substitution {
    pub fn new(table: HashMap<TypeVar, MonoType>) -> Self {
        Self {
            table,
            cache: HashMap::new(),
        }
    }

    pub fn empty() -> Self {
        Self::default()
    }

    pub fn get(&self, tv: &TypeVar) -> Option<&MonoType> {
        self.cache.get(tv).or_else(|| self.table.get(tv))
    }

    pub fn insert(&mut self, tv: TypeVar, ty: MonoType) -> Option<MonoType> {
        self.table.insert(tv, ty)
    }

    pub fn cache(&mut self, tv: TypeVar, ty: &MonoType) {
        self.cache
            .entry(tv)
            .and_modify(|stored| {
                if stored != ty {
                    *stored = ty.clone();
                }
            })
            .or_insert_with(|| ty.clone());
    }

    // pub fn apply<N>(&mut self, node: &mut N)
    // where
    //     N: Visitable,
    // {
    //     node.visit_mut_by(self);
    // }

    pub fn clear(&mut self) {
        self.table.clear();
        self.cache.clear();
    }
}

// impl VisitorMut for Substitution {
//     type BreakValue = !;

//     fn visit_ty_mut(&mut self, ty: &mut MonoType) -> ControlFlow<Self::BreakValue> {
//         ty.apply_mut(self);
//         ControlFlow::Continue(())
//     }
// }

/// A type is `Substitutable` if a substitution can be applied to it.
pub trait Substitutable: Sized {
    /// Apply a substitution to a type variable.
    fn apply(self, s: &mut Substitution) -> Self {
        self.try_apply(s).unwrap_or(self)
    }

    /// Apply a substitution to a type variable.
    fn apply_mut(&mut self, s: &mut Substitution) {
        if let Some(new) = self.try_apply(s) {
            *self = new;
        }
    }

    /// Apply a substitution to a type variable.
    fn apply_cow(&self, s: &mut Substitution) -> Cow<'_, Self>
    where
        Self: Clone,
    {
        match self.try_apply(s) {
            Some(t) => Cow::Owned(t),
            None => Cow::Borrowed(self),
        }
    }

    /// Apply a non-mutating substitution to a type variable.
    /// Should return `None` if there was nothing to apply
    /// which allows for optimizations.
    fn try_apply(&self, s: &mut Substitution) -> Option<Self>;
}

impl<T> Substitutable for Vec<T>
where
    T: Substitutable,
{
    fn apply(self, s: &mut Substitution) -> Self {
        self.into_iter().map(|t| t.apply(s)).collect()
    }

    fn apply_mut(&mut self, s: &mut Substitution) {
        for t in self {
            t.apply_mut(s);
        }
    }

    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        // divide and conquer merge ?
        todo!()
    }
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
