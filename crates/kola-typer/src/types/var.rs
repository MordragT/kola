use serde::{Deserialize, Serialize};
use std::{
    fmt,
    sync::atomic::{AtomicU32, Ordering},
};

use super::{MonoType, TypeClass, Typed};
use crate::{
    env::TypeClassEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution},
    types::Kind,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeVar {
    id: u32,
    kind: Kind,
}

// TODO this is just to make the compiler happy, the actual implementation of try_apply is under here
// This should be removed and handled in some better way, but is required,
// because TypeVar is inside the TypePhase and therefore to implement Substitutable for TypedNodes
// this is also required.
impl Substitutable for TypeVar {
    fn try_apply(&self, _s: &mut Substitution) -> Option<Self> {
        None
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            Kind::Type => write!(f, "'t{}", self.id),
            Kind::Record => write!(f, "'r{}", self.id),
            Kind::Label => write!(f, "'l{}", self.id),
            Kind::Tag => write!(f, "'g{}", self.id),
        }
    }
}
/// Efficient generalization with levels
/// https://okmij.org/ftp/ML/generalization.html#levels
// static LEVEL: AtomicU32 = AtomicU32::new(0);
static GENERATOR: AtomicU32 = AtomicU32::new(0);

impl TypeVar {
    pub fn new(kind: Kind) -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
        Self { id, kind }
    }

    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn kind(&self) -> Kind {
        self.kind
    }

    /// Set the kind of the TypeVar
    /// DANGER: This should only be used when you know what you're doing,
    pub fn set_kind(&mut self, kind: Kind) {
        self.kind = kind;
    }

    pub fn fresh(&self) -> Self {
        // Generate a new TypeVar with the same kind but a new id
        Self {
            id: GENERATOR.fetch_add(1, Ordering::Relaxed),
            kind: self.kind,
        }
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
        Self::new(Kind::Type)
    }
}

impl Typed for TypeVar {
    fn constrain(&self, with: TypeClass, env: &mut TypeClassEnv) -> Result<(), TypeError> {
        env.entry(*self)
            .and_modify(|constraint| constraint.push(with))
            .or_insert_with(|| vec![with]);
        Ok(())
    }
}
