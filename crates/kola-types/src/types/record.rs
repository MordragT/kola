use std::fmt;

use serde::{Deserialize, Serialize};

use crate::{
    class::{CheckClass, TypeClass, TypeClassEnv, TypeClassError},
    kind::{CheckKind, Kind},
    substitute::{Substitutable, Substitution},
};

use super::{MergeError, Row, Typed};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RecordType(pub Row);

impl RecordType {
    pub fn merge_left(&self, other: &Self) -> Result<Self, MergeError> {
        let merged_row = self.0.merge_left(&other.0)?;
        Ok(Self(merged_row))
    }

    pub fn merge_right(&self, other: &Self) -> Result<Self, MergeError> {
        let merged_row = self.0.merge_right(&other.0)?;
        Ok(Self(merged_row))
    }

    pub fn merge_deep(&self, other: &Self) -> Result<Self, MergeError> {
        let merged_row = self.0.merge_deep(&other.0)?;
        Ok(Self(merged_row))
    }
}

impl fmt::Display for RecordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Record {}", self.0)
    }
}

impl CheckKind for RecordType {
    fn kind(&self) -> Kind {
        Kind::Type
    }
}

impl CheckClass for RecordType {
    fn check_class(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeClassError> {
        match with {
            TypeClass::Comparable | TypeClass::Equatable => Ok(()),
            _ => Err(TypeClassError {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Typed for RecordType {}

impl Substitutable for RecordType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}
