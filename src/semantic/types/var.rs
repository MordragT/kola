use std::{
    fmt,
    sync::atomic::{AtomicU32, Ordering},
};

use serde::{Deserialize, Serialize};

use crate::semantic::{error::SemanticError, Substitutable, Substitution};

use super::{Kind, MonoType, Typed};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeVar {
    id: u32,
    level: u32,
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'t{}", self.id)
    }
}
/// Efficient generalization with levels
/// https://okmij.org/ftp/ML/generalization.html#levels
static LEVEL: AtomicU32 = AtomicU32::new(0);
static GENERATOR: AtomicU32 = AtomicU32::new(0);

impl TypeVar {
    // pub fn unchecked_new(id: u32) -> Self {
    //     let level = Self::load_level();
    //     Self { id, level }
    // }

    pub fn new() -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
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
        LEVEL.load(Ordering::Relaxed)
    }

    pub fn branch<F, T>(mut f: F) -> T
    where
        F: FnMut() -> T,
    {
        LEVEL.fetch_add(1, Ordering::Relaxed);
        let result = f();
        LEVEL.fetch_sub(1, Ordering::Relaxed);
        result
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

impl Typed for TypeVar {
    fn constrain(&self, with: Kind, s: &mut Substitution) -> Result<(), SemanticError> {
        s.constraints_entry(self)
            .and_modify(|constraint| constraint.push(with))
            .or_insert_with(|| vec![with]);
        Ok(())
    }
}
