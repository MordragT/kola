use kola_builtins::TypeProtocol;
use serde::{Deserialize, Serialize};
use std::fmt;

use super::{Kind, Typed};
use crate::{env::KindEnv, error::TypeError};

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
    fn constrain(&self, with: Kind, _env: &mut KindEnv) -> Result<(), TypeError> {
        match self {
            PrimitiveType::Unit => Err(TypeError::CannotConstrain {
                expected: with,
                actual: self.into(),
            }),
            PrimitiveType::Bool => match with {
                Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(TypeError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
            PrimitiveType::Num => match with {
                Kind::Addable | Kind::Comparable | Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(TypeError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
            PrimitiveType::Char => match with {
                Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(TypeError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
            PrimitiveType::Str => match with {
                Kind::Addable | Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(TypeError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
        }
    }
}
