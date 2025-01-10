use std::{
    fmt,
    sync::atomic::{AtomicU32, Ordering},
};

use crate::semantic::{
    error::{InferError, InferResult},
    Cache, Constraints, Context, Substitutable, Substitution, Unify,
};

use super::{Kind, MonoType, Typed};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar {
    id: u32,
    level: u32,
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'t{}", self.id)
    }
}

impl TypeVar {
    const GENERATOR: AtomicU32 = AtomicU32::new(0);

    /// Efficient generalization with levels
    /// https://okmij.org/ftp/ML/generalization.html#levels
    const LEVEL: AtomicU32 = AtomicU32::new(0);

    // pub fn unchecked_new(id: u32) -> Self {
    //     let level = Self::load_level();
    //     Self { id, level }
    // }

    pub fn new() -> Self {
        let id = Self::GENERATOR.fetch_add(1, Ordering::Relaxed);
        let level = Self::load_level();
        Self { id, level }
    }

    pub fn level(&self) -> u32 {
        self.level
    }

    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn load_level() -> u32 {
        Self::LEVEL.load(Ordering::Relaxed)
    }

    pub fn branch<F, T>(mut f: F) -> T
    where
        F: FnMut() -> T,
    {
        Self::LEVEL.fetch_add(1, Ordering::Relaxed);
        let result = f();
        Self::LEVEL.fetch_sub(1, Ordering::Relaxed);
        result
    }

    pub fn try_apply(&self, s: &mut Substitution, cache: &mut Cache) -> Option<MonoType> {
        let ty = cache.get(self).or_else(|| s.get(self)).cloned();
        ty.map(|mut ty| {
            ty.apply_mut(s, cache);
            cache
                .entry(*self)
                .and_modify(|stored| {
                    if stored != &ty {
                        *stored = ty.clone();
                    }
                })
                .or_insert_with(|| ty.clone());
            ty
        })
    }
}

impl Typed for TypeVar {
    fn constrain(&self, with: Kind, constraints: &mut Constraints) -> InferResult<()> {
        constraints
            .entry(*self)
            .and_modify(|constraint| constraint.push(with))
            .or_insert_with(|| vec![with]);
        Ok(())
    }

    fn contains(&self, tv: TypeVar) -> bool {
        *self == tv
    }

    fn type_vars(&self, vars: &mut Vec<TypeVar>) {
        vars.push(*self)
    }
}

impl Unify<&Self> for TypeVar {
    fn unify(&self, with: &Self, ctx: &mut Context) {
        if self != with {
            // ctx.error(InferError::CannotUnify {
            //     expected: self.into(),
            //     actual: with.into(),
            // })

            // in former apply path compression via cache is already implemented
            // so this should not become essentially an inefficient linked list
            ctx.substitution.insert(*self, MonoType::Var(*with));
        }
    }
}

impl Unify<&MonoType> for TypeVar {
    fn unify(&self, with: &MonoType, ctx: &mut Context) {
        if let MonoType::Var(with) = with {
            self.unify(with, ctx);
        } else if with.contains(*self) {
            ctx.error(InferError::Occurs(*self));
        } else {
            ctx.substitution.insert(*self, with.clone());
        }
    }
}
