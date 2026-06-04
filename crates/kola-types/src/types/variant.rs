use std::fmt;

use serde::{Deserialize, Serialize};

use crate::{
    class::{CheckClass, TypeClass, TypeClassEnv, TypeClassError},
    kind::{CheckKind, Kind},
    substitute::{Substitutable, Substitution},
};

use super::{Row, Typed};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VariantType(pub Row);

impl fmt::Display for VariantType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Variant {}", self.0)
    }
}

impl CheckKind for VariantType {
    fn kind(&self) -> Kind {
        Kind::Type
    }
}

impl CheckClass for VariantType {
    fn check_class(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeClassError> {
        match with {
            TypeClass::Equatable => Ok(()),
            _ => Err(TypeClassError {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Typed for VariantType {}

impl Substitutable for VariantType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}
