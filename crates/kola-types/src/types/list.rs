use serde::{Deserialize, Serialize};
use std::fmt;

use crate::{
    class::{CheckClass, TypeClass, TypeClassEnv, TypeClassError},
    kind::{CheckKind, Kind},
    substitute::{Substitutable, Substitution},
};

use super::{MonoType, Typed};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ListType(pub MonoType);

impl fmt::Display for ListType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "List {}", self.0)
    }
}

impl CheckKind for ListType {
    fn kind(&self) -> crate::kind::Kind {
        Kind::Type
    }
}

impl CheckClass for ListType {
    fn check_class(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeClassError> {
        match with {
            TypeClass::Addable | TypeClass::Comparable | TypeClass::Equatable => Ok(()),
            _ => Err(TypeClassError {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Typed for ListType {}

impl Substitutable for ListType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}
