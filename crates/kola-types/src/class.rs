use std::fmt;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{
    substitute::{Substitutable, Substitution},
    types::{MonoType, TypeVar},
};

pub type TypeClassEnv = IndexMap<TypeVar, Vec<TypeClass>>;

/// Represents a constraint on a type variable to a specific type class.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum TypeClass {
    Addable,
    Comparable,
    Equatable,
    Stringable,
}

impl Substitutable for TypeClass {
    fn try_apply(&self, _s: &mut Substitution) -> Option<Self> {
        None
    }
}

impl fmt::Display for TypeClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeClass::Addable => write!(f, "addable"),
            TypeClass::Comparable => write!(f, "comparable"),
            TypeClass::Equatable => write!(f, "equatable"),
            TypeClass::Stringable => write!(f, "stringable"),
        }
    }
}

#[derive(Debug, Clone, Error, PartialEq, Eq)]
#[error("Cannot Constrain Class: {expected} {actual}")]
pub struct TypeClassError {
    pub expected: TypeClass,
    pub actual: MonoType,
}

pub trait CheckClass {
    fn check_class(&self, with: TypeClass, env: &mut TypeClassEnv) -> Result<(), TypeClassError>;
}
