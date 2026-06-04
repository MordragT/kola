use derive_more::From;
use enum_as_inner::EnumAsInner;
use serde::{Deserialize, Serialize};
use std::fmt;

use super::{
    CompType, FuncType, LabelOrVar, ListType, PolyType, PrimitiveType, RecordType, Row,
    TypeConversionError, TypeVar, Typed, VariantType, WitType,
};
use crate::{
    class::{CheckClass, TypeClass, TypeClassEnv, TypeClassError},
    kind::{CheckKind, Kind},
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
    Record(Box<RecordType>),
    Variant(Box<VariantType>),
    Wit(Box<WitType>),
    Var(TypeVar),
    Row(Box<Row>),
    Label(LabelOrVar),
}

impl MonoType {
    pub const BOOL: Self = Self::Primitive(PrimitiveType::Bool);
    pub const NUM: Self = Self::Primitive(PrimitiveType::Num);
    pub const CHAR: Self = Self::Primitive(PrimitiveType::Char);
    pub const STR: Self = Self::Primitive(PrimitiveType::Str);
    pub const UNIT: Self = Self::Primitive(PrimitiveType::Unit);
}

impl MonoType {
    pub fn func(arg: Self, ret: impl Into<CompType>) -> Self {
        Self::Func(Box::new(FuncType::new(arg, ret.into())))
    }

    pub fn list(el: Self) -> Self {
        Self::List(Box::new(ListType(el)))
    }

    pub fn record(fields: Row) -> Self {
        Self::Record(Box::new(RecordType(fields)))
    }

    pub fn variant(cases: Row) -> Self {
        Self::Variant(Box::new(VariantType(cases)))
    }

    pub fn var() -> Self {
        Self::Var(TypeVar::new(Kind::Type))
    }

    pub fn label(label: impl Into<LabelOrVar>) -> Self {
        Self::Label(label.into())
    }

    pub fn wit(ty: Self) -> Self {
        Self::Wit(Box::new(WitType(ty)))
    }
}

impl MonoType {
    /// Takes a type with type vars inside and returns a polytype, with the type vars generalized inside the forall
    pub fn generalize(&self, bound: &[TypeVar]) -> PolyType {
        let forall = self.free_vars(bound);

        PolyType {
            forall,
            ty: self.clone(),
        }
    }
}

impl CheckKind for MonoType {
    fn kind(&self) -> Kind {
        match self {
            Self::Primitive(b) => b.kind(),
            Self::Func(f) => f.kind(),
            Self::List(l) => l.kind(),
            Self::Record(r) => r.kind(),
            Self::Variant(v) => v.kind(),
            Self::Row(r) => r.kind(),
            Self::Wit(w) => w.kind(),
            Self::Var(v) => v.kind(),
            Self::Label(l) => l.kind(),
        }
    }
}

impl CheckClass for MonoType {
    fn check_class(&self, with: TypeClass, env: &mut TypeClassEnv) -> Result<(), TypeClassError> {
        match self {
            Self::Primitive(b) => b.check_class(with, env),
            Self::Func(f) => f.check_class(with, env),
            Self::List(l) => l.check_class(with, env),
            Self::Record(r) => r.check_class(with, env),
            Self::Variant(v) => v.check_class(with, env),
            Self::Row(r) => r.check_class(with, env),
            Self::Wit(w) => w.check_class(with, env),
            Self::Var(v) => v.check_class(with, env),
            Self::Label(_) => Err(TypeClassError {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Typed for MonoType {}

impl Substitutable for MonoType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        match self {
            Self::Primitive(_) => None,
            Self::Func(f) => f.try_apply(s).map(Into::into),
            Self::List(l) => l.try_apply(s).map(Into::into),
            Self::Variant(v) => v.try_apply(s).map(Into::into),
            Self::Record(r) => r.try_apply(s).map(Into::into),
            Self::Row(r) => r.try_apply(s).map(Into::into),
            Self::Wit(w) => w.try_apply(s).map(Into::into),
            Self::Var(v) => v.try_apply(s),
            Self::Label(_) => None,
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
            Self::Record(r) => r.fmt(f),
            Self::Variant(v) => v.fmt(f),
            Self::Row(r) => r.fmt(f),
            Self::Var(tv) => tv.fmt(f),
            Self::Label(l) => l.fmt(f),
            Self::Wit(w) => w.fmt(f),
        }
    }
}

impl Default for MonoType {
    fn default() -> Self {
        Self::Var(TypeVar::default())
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

impl From<RecordType> for MonoType {
    fn from(value: RecordType) -> Self {
        Self::Record(Box::new(value))
    }
}

impl From<VariantType> for MonoType {
    fn from(value: VariantType) -> Self {
        Self::Variant(Box::new(value))
    }
}

impl From<Row> for MonoType {
    fn from(value: Row) -> Self {
        Self::Row(Box::new(value))
    }
}

impl From<WitType> for MonoType {
    fn from(value: WitType) -> Self {
        Self::Wit(Box::new(value))
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
