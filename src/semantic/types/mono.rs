use std::fmt;

use serde::{Deserialize, Serialize};

use crate::semantic::{Substitutable, Substitution};

use super::{BuiltinType, FuncType, PolyType, RecordType, TypeVar, Typed};

/// MonoType
/// Non-polymorphic types (e.g. `α → β`, `int → bool`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Monotypes
/// τ ::= α | gn τ1 .. τn
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MonoType {
    Builtin(BuiltinType),
    Func(Box<FuncType>),
    Record(Box<RecordType>),
    Var(TypeVar),
}

impl MonoType {
    pub const BOOL: Self = Self::Builtin(BuiltinType::Bool);
    pub const NUM: Self = Self::Builtin(BuiltinType::Num);
    pub const CHAR: Self = Self::Builtin(BuiltinType::Char);
    pub const STR: Self = Self::Builtin(BuiltinType::Str);
}

impl MonoType {
    pub fn variable() -> Self {
        Self::Var(TypeVar::new())
    }

    pub fn func(arg: Self, ret: Self) -> Self {
        Self::Func(Box::new(FuncType::new(arg, ret)))
    }
}

impl MonoType {
    pub fn into_var(self) -> Option<TypeVar> {
        match self {
            Self::Var(tv) => Some(tv),
            _ => None,
        }
    }

    pub fn into_const(self) -> Option<BuiltinType> {
        match self {
            Self::Builtin(tc) => Some(tc),
            _ => None,
        }
    }

    pub fn into_func(self) -> Option<FuncType> {
        match self {
            Self::Func(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_var(&self) -> Option<&TypeVar> {
        match self {
            Self::Var(tv) => Some(tv),
            _ => None,
        }
    }

    pub fn as_const(&self) -> Option<&BuiltinType> {
        match self {
            Self::Builtin(tc) => Some(tc),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<&FuncType> {
        match self {
            Self::Func(f) => Some(f),
            _ => None,
        }
    }

    pub fn is_var(&self) -> bool {
        self.as_var().is_some()
    }

    pub fn is_const(&self) -> bool {
        self.as_const().is_some()
    }

    pub fn is_func(&self) -> bool {
        self.as_func().is_some()
    }
}

impl MonoType {
    /// Takes a type with type vars inside and returns a polytype, with the type vars generalized inside the forall
    pub fn generalize(&self, bound: &[TypeVar]) -> PolyType {
        let vars = self.free_vars(bound);

        PolyType {
            vars,
            ty: self.clone(),
        }
    }
}

impl Typed for MonoType {
    fn constrain(
        &self,
        with: super::Kind,
        s: &mut Substitution,
    ) -> Result<(), crate::semantic::error::SemanticError> {
        match self {
            Self::Builtin(b) => b.constrain(with, s),
            Self::Func(func) => func.constrain(with, s),
            Self::Record(r) => r.constrain(with, s),
            Self::Var(tv) => tv.constrain(with, s),
        }
    }
}

impl Substitutable for MonoType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        match self {
            Self::Builtin(_) => None,
            Self::Func(f) => f.try_apply(s).map(Into::into),
            Self::Record(r) => r.try_apply(s).map(Into::into),
            Self::Var(tv) => tv.try_apply(s),
        }
    }
}

impl fmt::Display for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Builtin(b) => b.fmt(f),
            Self::Func(func) => func.fmt(f),
            Self::Record(r) => r.fmt(f),
            Self::Var(tv) => tv.fmt(f),
        }
    }
}

impl From<BuiltinType> for MonoType {
    fn from(value: BuiltinType) -> Self {
        Self::Builtin(value)
    }
}

impl From<&BuiltinType> for MonoType {
    fn from(value: &BuiltinType) -> Self {
        Self::Builtin(*value)
    }
}

impl From<FuncType> for MonoType {
    fn from(value: FuncType) -> Self {
        Self::Func(Box::new(value))
    }
}

impl From<RecordType> for MonoType {
    fn from(value: RecordType) -> Self {
        Self::Record(Box::new(value))
    }
}

impl From<TypeVar> for MonoType {
    fn from(value: TypeVar) -> Self {
        Self::Var(value)
    }
}
