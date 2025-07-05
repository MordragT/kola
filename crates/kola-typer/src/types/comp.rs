use std::fmt::{self};

use serde::{Deserialize, Serialize};

use crate::{
    prelude::{Substitutable, Substitution},
    substitute::merge,
    types::{MonoType, RowType},
};

/// Computation type
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CompType {
    pub ty: MonoType,
    pub effect: RowType,
}

impl CompType {
    pub fn pure(ty: MonoType) -> Self {
        Self {
            ty,
            effect: RowType::Empty,
        }
    }

    pub fn new(ty: MonoType, effect: RowType) -> Self {
        Self { ty, effect }
    }
}

impl fmt::Display for CompType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { ty, effect } = self;

        write!(f, "{ty} ~ {effect}")
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
