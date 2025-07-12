use kola_protocol::TypeProtocol;
use serde::{Deserialize, Serialize};
use std::fmt;

use super::{Kind, TypeClass, Typed};
use crate::{env::TypeClassEnv, error::TypeError};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PrimitiveType {
    Unit,
    Bool,
    Num,
    Char,
    Str,
}

impl PrimitiveType {
    pub fn to_protocol(&self) -> TypeProtocol {
        match self {
            PrimitiveType::Unit => TypeProtocol::Unit,
            PrimitiveType::Bool => TypeProtocol::Bool,
            PrimitiveType::Num => TypeProtocol::Num,
            PrimitiveType::Char => TypeProtocol::Char,
            PrimitiveType::Str => TypeProtocol::Str,
        }
    }
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

impl Typed for PrimitiveType {
    fn kind(&self) -> Kind {
        Kind::Type
    }

    fn constrain(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeError> {
        match self {
            PrimitiveType::Unit => Err(TypeError::CannotConstrain {
                expected: with,
                actual: self.into(),
            }),
            PrimitiveType::Bool => match with {
                TypeClass::Equatable | TypeClass::Stringable => Ok(()),
                _ => Err(TypeError::CannotConstrain {
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
                _ => Err(TypeError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
            PrimitiveType::Str => match with {
                TypeClass::Addable | TypeClass::Equatable | TypeClass::Stringable => Ok(()),
                _ => Err(TypeError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
        }
    }
}
