use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_protocol::{KindProtocol, TypeProtocol};
use kola_utils::interner::{StrInterner, StrKey};
use serde::{Deserialize, Serialize};
use std::fmt;

use super::{
    CompType, FuncType, Kind, Label, LabeledType, ListType, PolyType, PrimitiveType, RowType,
    TypeVar, Typed, Wit,
};
use crate::{
    env::TypeClassEnv,
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
    Var(TypeVar),
    Row(Box<RowType>),
    Label(Label),
    Wit(Box<Wit>),
}

impl MonoType {
    pub const BOOL: Self = Self::Primitive(PrimitiveType::Bool);
    pub const NUM: Self = Self::Primitive(PrimitiveType::Num);
    pub const CHAR: Self = Self::Primitive(PrimitiveType::Char);
    pub const STR: Self = Self::Primitive(PrimitiveType::Str);
    pub const UNIT: Self = Self::Primitive(PrimitiveType::Unit);
}

impl MonoType {
    pub fn func(arg: Self, ret: CompType) -> Self {
        Self::Func(Box::new(FuncType::new(arg, ret)))
    }

    pub fn pure_func(arg: Self, ret: Self) -> Self {
        Self::Func(Box::new(FuncType::new(arg, CompType::pure(ret))))
    }

    pub fn list(el: Self) -> Self {
        Self::List(Box::new(ListType(el)))
    }

    pub fn row(head: LabeledType, tail: Self) -> Self {
        Self::Row(Box::new(RowType::Extension { head, tail }))
    }

    pub fn empty_row() -> Self {
        Self::Row(Box::new(RowType::Empty))
    }

    pub fn variable(kind: Kind) -> Self {
        Self::Var(TypeVar::new(kind))
    }

    pub fn label(label: StrKey) -> Self {
        Self::Label(Label(label))
    }

    pub fn wit(ty: Self) -> Self {
        Self::Wit(Box::new(Wit(ty)))
    }

    pub fn from_protocol(
        proto: TypeProtocol,
        bound: &mut Vec<TypeVar>,
        interner: &mut StrInterner,
    ) -> Self {
        let this = match proto {
            TypeProtocol::Unit => Self::UNIT,
            TypeProtocol::Bool => Self::BOOL,
            TypeProtocol::Num => Self::NUM,
            TypeProtocol::Char => Self::CHAR,
            TypeProtocol::Str => Self::STR,
            TypeProtocol::List(el) => Self::list(Self::from_protocol(*el, bound, interner)),
            TypeProtocol::Func(arg, ret) => Self::func(
                Self::from_protocol(*arg, bound, interner),
                CompType::from_protocol(*ret, bound, interner),
            ),
            TypeProtocol::Record(fields) => {
                let mut row = Self::empty_row();
                for (label, ty) in fields.into_iter().rev() {
                    let labeled = LabeledType::new(
                        Label(interner.intern(label)),
                        Self::from_protocol(ty, bound, interner),
                    );
                    row = Self::row(labeled, row);
                }
                row
            }
            TypeProtocol::Variant(tags) => {
                let mut row = Self::empty_row();
                for (label, ty) in tags.into_iter().rev() {
                    let labeled = LabeledType::new(
                        Label(interner.intern(label)),
                        Self::from_protocol(ty, bound, interner),
                    );
                    row = Self::row(labeled, row);
                }
                row
            }
            TypeProtocol::Label(label) => Self::label(interner.intern(label)),
            TypeProtocol::Var(id, kind) => {
                let var = &mut bound[id as usize];

                let var = match (var.kind(), kind) {
                    (Kind::Type, KindProtocol::Type)
                    | (Kind::Record, KindProtocol::Record)
                    | (Kind::Label, KindProtocol::Label)
                    | (Kind::Tag, KindProtocol::Tag) => *var,
                    (Kind::Type, _) => {
                        var.set_kind(kind.into());
                        *var
                    }
                    _ => panic!(
                        "Cannot convert type var with kind {:?} to kind {:?}",
                        var.kind(),
                        kind
                    ),
                };

                MonoType::Var(var)
            }
            TypeProtocol::Witness(proto) => {
                let ty = Self::from_protocol(*proto, bound, interner);
                Self::wit(ty)
            }
        };

        this
    }

    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        match self {
            Self::Primitive(prim) => prim.to_protocol(),
            Self::Func(func) => func.to_protocol(interner),
            Self::List(list) => list.to_protocol(interner),
            Self::Row(row) => row.to_protocol(interner),
            Self::Label(label) => label.to_protocol(interner),
            Self::Wit(wit) => wit.to_protocol(interner),
            Self::Var(_) => todo!(),
        }
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
        let forall = self.free_vars(bound);

        PolyType {
            forall,
            ty: self.clone(),
        }
    }
}

impl Typed for MonoType {
    fn constrain(&self, with: super::TypeClass, env: &mut TypeClassEnv) -> Result<(), TypeError> {
        match self {
            Self::Primitive(b) => b.constrain(with, env),
            Self::Func(f) => f.constrain(with, env),
            Self::List(l) => l.constrain(with, env),
            Self::Row(r) => r.constrain(with, env),
            Self::Wit(w) => w.constrain(with, env),
            Self::Var(v) => v.constrain(with, env),
            _ => Err(TypeError::CannotConstrain {
                expected: with,
                actual: self.clone().into(),
            }),
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
            Self::Wit(w) => w.try_apply(s).map(Into::into),
            Self::Var(v) => v.try_apply(s),
            _ => None,
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

impl From<RowType> for MonoType {
    fn from(value: RowType) -> Self {
        Self::Row(Box::new(value))
    }
}

impl From<Wit> for MonoType {
    fn from(value: Wit) -> Self {
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
