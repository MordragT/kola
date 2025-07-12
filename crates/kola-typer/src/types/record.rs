use std::fmt;

use kola_protocol::TypeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};

use crate::{
    env::TypeClassEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution},
};

use super::{Kind, Row, TypeClass, Typed};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RecordType(pub Row);

impl RecordType {
    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        let fields = self.0.to_protocol(interner);
        TypeProtocol::Record(fields)
    }

    pub fn merge_left(&self, other: &Self) -> Result<Self, TypeError> {
        let merged_row = self.0.merge_left(&other.0)?;
        Ok(Self(merged_row))
    }

    pub fn merge_right(&self, other: &Self) -> Result<Self, TypeError> {
        let merged_row = self.0.merge_right(&other.0)?;
        Ok(Self(merged_row))
    }

    pub fn merge_deep(&self, other: &Self) -> Result<Self, TypeError> {
        let merged_row = self.0.merge_deep(&other.0)?;
        Ok(Self(merged_row))
    }
}

impl fmt::Display for RecordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Record {}", self.0)
    }
}

impl Typed for RecordType {
    fn kind(&self) -> Kind {
        Kind::Type
    }

    fn constrain(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeError> {
        match with {
            TypeClass::Comparable | TypeClass::Equatable => Ok(()),
            _ => Err(TypeError::CannotConstrain {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Substitutable for RecordType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}
