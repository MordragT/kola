use serde::{Deserialize, Serialize};
use std::fmt;

use crate::{
    class::{CheckClass, TypeClass, TypeClassEnv, TypeClassError},
    kind::{CheckKind, Kind},
};

use super::Typed;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PrimitiveType {
    Unit,
    Bool,
    Num,
    Char,
    Str,
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "Unit"),
            Self::Bool => write!(f, "Bool"),
            Self::Num => write!(f, "Num"),
            Self::Char => write!(f, "Char"),
            Self::Str => write!(f, "Str"),
        }
    }
}

impl CheckKind for PrimitiveType {
    fn kind(&self) -> Kind {
        Kind::Type
    }
}

impl CheckClass for PrimitiveType {
    fn check_class(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeClassError> {
        match self {
            PrimitiveType::Unit => Err(TypeClassError {
                expected: with,
                actual: self.into(),
            }),
            PrimitiveType::Bool => match with {
                TypeClass::Equatable | TypeClass::Stringable => Ok(()),
                _ => Err(TypeClassError {
                    expected: with,
                    actual: self.into(),
                }),
            },
            PrimitiveType::Num => match with {
                TypeClass::Addable
                | TypeClass::Comparable
                | TypeClass::Equatable
                | TypeClass::Stringable => Ok(()),
            },
            PrimitiveType::Char => match with {
                TypeClass::Equatable | TypeClass::Stringable => Ok(()),
                _ => Err(TypeClassError {
                    expected: with,
                    actual: self.into(),
                }),
            },
            PrimitiveType::Str => match with {
                TypeClass::Addable | TypeClass::Equatable | TypeClass::Stringable => Ok(()),
                _ => Err(TypeClassError {
                    expected: with,
                    actual: self.into(),
                }),
            },
        }
    }
}

impl Typed for PrimitiveType {}
