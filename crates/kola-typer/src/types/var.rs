use serde::{Deserialize, Serialize};
use std::{
    fmt,
    sync::atomic::{AtomicU32, Ordering},
};

use super::{Kind, MonoType, Typed};
use crate::{
    env::KindEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution},
};

// Maybe this should also convey that it is always free ? So maybe rename to FreeVar ?
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct KindedVar {
    pub var: TypeVar,
    pub kind: Kind,
}

impl KindedVar {
    /// Constructor that takes both a TypeVar and a Kind.
    pub fn new(var: TypeVar, kind: Kind) -> Self {
        KindedVar { var, kind }
    }

    /// Constructor that generates a new TypeVar and takes a Kind as input.
    pub fn with_kind(kind: Kind) -> Self {
        KindedVar {
            var: TypeVar::fresh(),
            kind,
        }
    }

    /// Constructor that generates a new TypeVar and uses the default Kind.
    pub fn fresh() -> Self
    where
        Kind: Default,
    {
        KindedVar {
            var: TypeVar::fresh(),
            kind: Kind::default(),
        }
    }
}

impl fmt::Display for KindedVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.kind, self.var)
    }
}

// KindedVar returns None because it is only used inside quantifiers
// meaning they will/should never be substituted,
// because they are not bound variables in the type system.
impl Substitutable for KindedVar {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeVar(u32);

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'t{}", self.0)
    }
}
/// Efficient generalization with levels
/// https://okmij.org/ftp/ML/generalization.html#levels
// static LEVEL: AtomicU32 = AtomicU32::new(0);
static GENERATOR: AtomicU32 = AtomicU32::new(0);

impl TypeVar {
    pub fn fresh() -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
        Self(id)
    }

    pub fn id(&self) -> u32 {
        self.0
    }

    pub fn try_apply(&self, s: &mut Substitution) -> Option<MonoType> {
        let ty = s.get(self).cloned();
        ty.map(|mut ty| {
            ty.apply_mut(s);
            s.cache(*self, &ty);
            ty
        })
    }
}

impl Default for TypeVar {
    fn default() -> Self {
        Self::fresh()
    }
}

impl Typed for TypeVar {
    fn constrain(&self, with: Kind, env: &mut KindEnv) -> Result<(), TypeError> {
        env.entry(*self)
            .and_modify(|constraint| constraint.push(with))
            .or_insert_with(|| vec![with]);
        Ok(())
    }
}
