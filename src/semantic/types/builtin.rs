use std::fmt;

use crate::semantic::{
    error::{InferError, InferResult},
    Constraints, Context, Kind, Unify,
};

use super::{TypeVar, Typed};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

impl Unify<&Self> for BuiltinType {
    fn unify(&self, with: &Self, ctx: &mut Context) {
        if self != with {
            ctx.errors.push(InferError::CannotUnify {
                expected: self.into(),
                actual: with.into(),
            });
        }
    }
}

impl Typed for BuiltinType {
    fn constrain(&self, with: Kind, _: &mut Constraints) -> InferResult<()> {
        match self {
            Self::Bool => match with {
                Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(InferError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
            Self::Num => match with {
                Kind::Addable | Kind::Comparable | Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(InferError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
            Self::Char => match with {
                Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(InferError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
            Self::Str => match with {
                Kind::Addable | Kind::Equatable | Kind::Stringable => Ok(()),
                _ => Err(InferError::CannotConstrain {
                    expected: with,
                    actual: self.into(),
                }),
            },
        }
    }

    fn contains(&self, _: TypeVar) -> bool {
        false
    }

    fn type_vars(&self, _: &mut Vec<TypeVar>) {}
}
