use std::{
    collections::BTreeMap,
    fmt::{self},
};

use kola_protocol::TypeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};

use super::{MonoType, Row, TypeVar};
use crate::{
    error::TypeError,
    substitute::{Substitutable, Substitution, merge},
};

/// Computation type
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CompType {
    pub ty: MonoType,
    pub effect: Row,
}

impl CompType {
    pub fn pure(ty: MonoType) -> Self {
        Self {
            ty,
            effect: Row::Empty,
        }
    }

    pub fn new(ty: MonoType, effect: Row) -> Self {
        Self { ty, effect }
    }

    pub fn from_protocol(
        proto: TypeProtocol,
        bound: &mut BTreeMap<u32, TypeVar>,
        interner: &mut StrInterner,
    ) -> Result<Self, TypeError> {
        let ty = MonoType::from_protocol(proto, bound, interner)?;
        let effect = Row::Empty;

        Ok(Self { ty, effect })
    }

    // TODO also consider effects
    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        self.ty.to_protocol(interner)
    }
}

impl fmt::Display for CompType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { ty, effect } = self;

        ty.fmt(f)?;

        if *effect != Row::Empty {
            write!(f, " ~ {effect}")?;
        }

        Ok(())
    }
}

impl Substitutable for CompType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let ty = self.ty.try_apply(s);
        let effect = self.effect.try_apply(s);

        merge(ty, || self.ty.clone(), effect, || self.effect.clone())
            .map(|(ty, effect)| CompType { ty, effect })
    }
}

impl From<MonoType> for CompType {
    fn from(ty: MonoType) -> Self {
        Self::pure(ty)
    }
}
