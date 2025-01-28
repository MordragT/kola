use std::fmt;

use serde::{Deserialize, Serialize};

use crate::{KindEnv, error::SemanticError};

use super::{Kind, Typed};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BuiltinType {
    Bool,
    Num,
    Char,
    Str,
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "Bool"),
            Self::Num => write!(f, "Num"),
            Self::Char => write!(f, "Char"),
            Self::Str => write!(f, "Str"),
        }
    }
}

impl Typed for BuiltinType {
    fn constrain(&self, with: Kind, _env: &mut KindEnv) -> Result<(), SemanticError> {
        match self {
            BuiltinType::Bool => match with {
                Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(SemanticError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
            BuiltinType::Num => match with {
                Kind::Addable | Kind::Comparable | Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(SemanticError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
            BuiltinType::Char => match with {
                Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(SemanticError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
            BuiltinType::Str => match with {
                Kind::Addable | Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(SemanticError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
        }
    }
}
