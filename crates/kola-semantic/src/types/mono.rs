use derive_more::From;
use kola_utils::as_variant;
use serde::{Deserialize, Serialize};
use std::fmt;

use super::{BuiltinType, FuncType, ListType, PolyType, Property, RowType, TypeVar, Typed};
use crate::{
    env::KindEnv,
    error::SemanticError,
    substitute::{Substitutable, Substitution},
};

/// MonoType
/// Non-polymorphic types (e.g. `α → β`, `int → bool`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Monotypes
/// τ ::= α | gn τ1 .. τn
#[derive(Debug, From, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MonoType {
    Builtin(BuiltinType),
    Func(Box<FuncType>),
    List(Box<ListType>),
    Row(Box<RowType>),
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

    pub fn list(el: Self) -> Self {
        Self::List(Box::new(ListType::new(el)))
    }

    pub fn row(head: Property, tail: Self) -> Self {
        Self::Row(Box::new(RowType::Extension { head, tail }))
    }

    pub fn empty_row() -> Self {
        Self::Row(Box::new(RowType::Empty))
    }
}

impl MonoType {
    pub fn into_builtin(self) -> Option<BuiltinType> {
        as_variant!(self, Self::Builtin)
    }

    pub fn into_func(self) -> Option<FuncType> {
        as_variant!(self, Self::Func).map(Box::into_inner)
    }

    pub fn into_list(self) -> Option<ListType> {
        as_variant!(self, Self::List).map(Box::into_inner)
    }

    pub fn into_row(self) -> Option<RowType> {
        as_variant!(self, Self::Row).map(Box::into_inner)
    }

    pub fn into_var(self) -> Option<TypeVar> {
        as_variant!(self, Self::Var)
    }

    pub fn as_builtin(&self) -> Option<&BuiltinType> {
        as_variant!(self, Self::Builtin)
    }

    pub fn as_func(&self) -> Option<&FuncType> {
        as_variant!(self, Self::Func)
    }

    pub fn as_list(&self) -> Option<&ListType> {
        as_variant!(self, Self::List)
    }

    pub fn as_row(&self) -> Option<&RowType> {
        as_variant!(self, Self::Row)
    }

    pub fn as_var(&self) -> Option<&TypeVar> {
        as_variant!(self, Self::Var)
    }

    pub fn is_builtin(&self) -> bool {
        matches!(self, Self::Builtin(_))
    }

    pub fn is_func(&self) -> bool {
        matches!(self, Self::Func(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Self::List(_))
    }

    pub fn is_row(&self) -> bool {
        matches!(self, Self::Row(_))
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
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
    fn constrain(&self, with: super::Kind, env: &mut KindEnv) -> Result<(), SemanticError> {
        match self {
            Self::Builtin(b) => b.constrain(with, env),
            Self::Func(f) => f.constrain(with, env),
            Self::List(l) => l.constrain(with, env),
            Self::Row(r) => r.constrain(with, env),
            Self::Var(v) => v.constrain(with, env),
        }
    }
}

impl Substitutable for MonoType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        match self {
            Self::Builtin(_) => None,
            Self::Func(f) => f.try_apply(s).map(Into::into),
            Self::List(l) => l.try_apply(s).map(Into::into),
            Self::Row(r) => r.try_apply(s).map(Into::into),
            Self::Var(v) => v.try_apply(s),
        }
    }
}

impl fmt::Display for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Builtin(b) => b.fmt(f),
            Self::Func(func) => func.fmt(f),
            Self::List(l) => l.fmt(f),
            Self::Row(r) => r.fmt(f),
            Self::Var(tv) => tv.fmt(f),
        }
    }
}

impl Default for MonoType {
    fn default() -> Self {
        Self::variable()
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

impl From<RowType> for MonoType {
    fn from(value: RowType) -> Self {
        Self::Row(Box::new(value))
    }
}

impl From<ListType> for MonoType {
    fn from(value: ListType) -> Self {
        Self::List(Box::new(value))
    }
}
