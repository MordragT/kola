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
    pub fn new() -> Self {
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
        Self::new()
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
