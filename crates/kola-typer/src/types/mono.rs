use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_builtins::TypeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};
use std::fmt;

use super::{
    CompType, FuncType, LabeledType, ListType, PolyType, PrimitiveType, RowType, TypeRep, TypeVar,
    Typed,
};
use crate::{
    env::KindEnv,
    error::{TypeConversionError, TypeError},
    substitute::{Substitutable, Substitution},
};

/// MonoType
/// Non-polymorphic types (e.g. `α → β`, `int → bool`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Monotypes
/// τ ::= α | gn τ1 .. τn
#[derive(Debug, From, EnumAsInner, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MonoType {
    Primitive(PrimitiveType),
    Func(Box<FuncType>),
    List(Box<ListType>),
    Row(Box<RowType>),
    TypeRep(Box<TypeRep>),
    Var(TypeVar),
}

impl MonoType {
    pub const BOOL: Self = Self::Primitive(PrimitiveType::Bool);
    pub const NUM: Self = Self::Primitive(PrimitiveType::Num);
    pub const CHAR: Self = Self::Primitive(PrimitiveType::Char);
    pub const STR: Self = Self::Primitive(PrimitiveType::Str);
    pub const UNIT: Self = Self::Primitive(PrimitiveType::Unit);
}

impl MonoType {
    pub fn variable() -> Self {
        Self::Var(TypeVar::new())
    }

    pub fn func(arg: Self, ret: CompType) -> Self {
        Self::Func(Box::new(FuncType::new(arg, ret)))
    }

    pub fn pure_func(arg: Self, ret: Self) -> Self {
        Self::Func(Box::new(FuncType::new(arg, CompType::pure(ret))))
    }

    pub fn list(el: Self) -> Self {
        Self::List(Box::new(ListType::new(el)))
    }

    pub fn type_rep(ty: Self) -> Self {
        Self::TypeRep(Box::new(TypeRep::new(ty)))
    }

    pub fn row(head: LabeledType, tail: Self) -> Self {
        Self::Row(Box::new(RowType::Extension { head, tail }))
    }

    pub fn empty_row() -> Self {
        Self::Row(Box::new(RowType::Empty))
    }

    pub fn from_protocol(
        proto: TypeProtocol,
        bound_forall: &[TypeVar],
        bound_exists: &[TypeVar],
        interner: &mut StrInterner,
    ) -> Self {
        let this = match proto {
            TypeProtocol::Unit => Self::UNIT,
            TypeProtocol::Bool => Self::BOOL,
            TypeProtocol::Num => Self::NUM,
            TypeProtocol::Char => Self::CHAR,
            TypeProtocol::Str => Self::STR,
            TypeProtocol::List(el) => Self::list(Self::from_protocol(
                *el,
                bound_forall,
                bound_exists,
                interner,
            )),
            TypeProtocol::Record(fields) => {
                let mut row = Self::empty_row();
                for (label, ty) in fields.into_iter().rev() {
                    let labeled = LabeledType::new(
                        interner.intern(label),
                        Self::from_protocol(ty, bound_forall, bound_exists, interner),
                    );
                    row = Self::row(labeled, row);
                }
                row
            }
            TypeProtocol::Variant(tags) => {
                let mut row = Self::empty_row();
                for (label, ty) in tags.into_iter().rev() {
                    let labeled = LabeledType::new(
                        interner.intern(label),
                        Self::from_protocol(ty, bound_forall, bound_exists, interner),
                    );
                    row = Self::row(labeled, row);
                }
                row
            }
            TypeProtocol::Lambda(arg, ret) => Self::func(
                Self::from_protocol(*arg, bound_forall, bound_exists, interner),
                CompType::from_protocol(*ret, bound_forall, bound_exists, interner),
            ),
            TypeProtocol::ForallVar(id) => Self::Var(bound_forall[id as usize]),
            TypeProtocol::ExistsVar(id) => Self::Var(bound_exists[id as usize]),
        };

        this
    }

    /// Only works, if the left-hand side is a concrete row type.
    /// The right-hand side can be a row type or a row variable.
    pub fn merge_left(&self, other: &Self) -> Result<Self, TypeError> {
        if let Self::Row(row) = self {
            row.merge_left(other).map(Into::into)
        } else {
            return Err(TypeError::CannotMerge {
                lhs: self.clone(),
                rhs: other.clone(),
            });
        }
    }

    /// Only works, if the right-hand side is a concrete row type.
    /// The left-hand side can be a row type or a row variable.
    pub fn merge_right(&self, other: &Self) -> Result<Self, TypeError> {
        other.merge_left(self)
    }

    /// Only works, if both sides are concrete row types.
    pub fn merge_deep(&self, other: &Self) -> Result<Self, TypeError> {
        if let (Self::Row(r1), Self::Row(r2)) = (self, other) {
            r1.merge_deep(r2).map(Into::into)
        } else {
            Err(TypeError::CannotMerge {
                lhs: self.clone(),
                rhs: other.clone(),
            })
        }
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
    fn constrain(&self, with: super::Kind, env: &mut KindEnv) -> Result<(), TypeError> {
        match self {
            Self::Primitive(b) => b.constrain(with, env),
            Self::Func(f) => f.constrain(with, env),
            Self::List(l) => l.constrain(with, env),
            Self::Row(r) => r.constrain(with, env),
            Self::TypeRep(tr) => tr.constrain(with, env),
            Self::Var(v) => v.constrain(with, env),
        }
    }
}

impl Substitutable for MonoType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        match self {
            Self::Primitive(_) => None,
            Self::Func(f) => f.try_apply(s).map(Into::into),
            Self::List(l) => l.try_apply(s).map(Into::into),
            Self::Row(r) => r.try_apply(s).map(Into::into),
            Self::TypeRep(tr) => tr.try_apply(s).map(Into::into),
            Self::Var(v) => v.try_apply(s),
        }
    }
}

// TODO thread display through KindEnv and StrInterner

impl fmt::Display for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Primitive(b) => b.fmt(f),
            Self::Func(func) => func.fmt(f),
            Self::List(l) => l.fmt(f),
            Self::Row(r) => r.fmt(f),
            Self::TypeRep(tr) => tr.fmt(f),
            Self::Var(tv) => tv.fmt(f),
        }
    }
}

impl Default for MonoType {
    fn default() -> Self {
        Self::variable()
    }
}

impl From<&PrimitiveType> for MonoType {
    fn from(value: &PrimitiveType) -> Self {
        Self::Primitive(*value)
    }
}

impl From<FuncType> for MonoType {
    fn from(value: FuncType) -> Self {
        Self::Func(Box::new(value))
    }
}

impl From<ListType> for MonoType {
    fn from(value: ListType) -> Self {
        Self::List(Box::new(value))
    }
}

impl From<RowType> for MonoType {
    fn from(value: RowType) -> Self {
        Self::Row(Box::new(value))
    }
}

impl From<TypeRep> for MonoType {
    fn from(value: TypeRep) -> Self {
        Self::TypeRep(Box::new(value))
    }
}

impl TryFrom<PolyType> for MonoType {
    type Error = TypeConversionError;

    fn try_from(value: PolyType) -> Result<Self, Self::Error> {
        value.into_mono()
    }
}

impl<'a> TryFrom<&'a PolyType> for &'a MonoType {
    type Error = TypeConversionError;

    fn try_from(value: &'a PolyType) -> Result<Self, Self::Error> {
        value.as_mono()
    }
}
