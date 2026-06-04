use std::fmt;

use serde::{Deserialize, Serialize};

use super::{MonoType, Typed};
use crate::{
    class::{CheckClass, TypeClass, TypeClassEnv, TypeClassError},
    kind::{CheckKind, Kind},
    substitute::{Substitutable, Substitution},
};

/// Represents a type representation in the system
/// Useful for Type Reification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WitType(pub MonoType);

impl fmt::Display for WitType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeWit {}", self.0)
    }
}

impl CheckKind for WitType {
    fn kind(&self) -> Kind {
        Kind::Type
    }
}

impl CheckClass for WitType {
    fn check_class(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeClassError> {
        Err(TypeClassError {
            expected: with,
            actual: self.clone().into(),
        })
    }
}

impl Typed for WitType {}

impl Substitutable for WitType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}
