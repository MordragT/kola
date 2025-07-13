use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_protocol::TypeProtocol;
use kola_utils::interner::{StrInterner, StrKey};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, fmt};

use super::{
    CompType, FuncType, Kind, Label, LabeledType, ListType, PolyType, PrimitiveType, RecordType,
    Row, TypeVar, Typed, VariantType, WitType,
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
    Record(Box<RecordType>),
    Variant(Box<VariantType>),
    Wit(Box<WitType>),
    Var(TypeVar),
    Row(Box<Row>),
    Label(Label),
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

    pub fn label(label: StrKey) -> Self {
        Self::Label(Label(label))
    }

    pub fn wit(ty: Self) -> Self {
        Self::Wit(Box::new(WitType(ty)))
    }

    pub fn from_protocol(
        proto: TypeProtocol,
        bound: &mut BTreeMap<u32, TypeVar>,
        interner: &mut StrInterner,
    ) -> Result<Self, TypeError> {
        let this = match proto {
            TypeProtocol::Unit => Self::UNIT,
            TypeProtocol::Bool => Self::BOOL,
            TypeProtocol::Num => Self::NUM,
            TypeProtocol::Char => Self::CHAR,
            TypeProtocol::Str => Self::STR,
            TypeProtocol::List(el) => Self::list(Self::from_protocol(*el, bound, interner)?),
            TypeProtocol::Func(arg, ret) => Self::func(
                Self::from_protocol(*arg, bound, interner)?,
                CompType::from_protocol(*ret, bound, interner)?,
            ),
            TypeProtocol::Record(fields) => {
                let mut row = Row::Empty;
                for (label, ty) in fields.into_iter().rev() {
                    let head = LabeledType::new(
                        Label(interner.intern(label)),
                        Self::from_protocol(ty, bound, interner)?,
                    );
                    row = Row::extension(head, row);
                }
                Self::record(row)
            }
            TypeProtocol::Variant(tags) => {
                let mut row = Row::Empty;
                for (label, ty) in tags.into_iter().rev() {
                    let head = LabeledType::new(
                        Label(interner.intern(label)),
                        Self::from_protocol(ty, bound, interner)?,
                    );
                    row = Row::extension(head, row);
                }
                Self::variant(row)
            }
            TypeProtocol::Label(label) => Self::label(interner.intern(label)),
            TypeProtocol::Var(id, kind) => {
                let kind = Kind::from(kind);

                let var = if let Some(var) = bound.get(&id) {
                    var.check_kind(kind)?;
                    *var
                } else {
                    let var = TypeVar::new(kind);
                    bound.insert(id, var);
                    var
                };

                MonoType::Var(var)
            }
            TypeProtocol::Witness(proto) => {
                let ty = Self::from_protocol(*proto, bound, interner)?;
                Self::wit(ty)
            }
        };

        Ok(this)
    }

    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        match self {
            Self::Primitive(prim) => prim.to_protocol(),
            Self::Func(func) => func.to_protocol(interner),
            Self::List(list) => list.to_protocol(interner),
            Self::Record(record) => record.to_protocol(interner),
            Self::Variant(variant) => variant.to_protocol(interner),
            Self::Label(label) => label.to_protocol(interner),
            Self::Wit(wit) => wit.to_protocol(interner),
            Self::Var(_) => todo!(),
            Self::Row(_) => unimplemented!(),
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

    fn constrain_class(
        &self,
        with: super::TypeClass,
        env: &mut TypeClassEnv,
    ) -> Result<(), TypeError> {
        match self {
            Self::Primitive(b) => b.constrain_class(with, env),
            Self::Func(f) => f.constrain_class(with, env),
            Self::List(l) => l.constrain_class(with, env),
            Self::Record(r) => r.constrain_class(with, env),
            Self::Variant(v) => v.constrain_class(with, env),
            Self::Row(r) => r.constrain_class(with, env),
            Self::Wit(w) => w.constrain_class(with, env),
            Self::Var(v) => v.constrain_class(with, env),
            Self::Label(_) => Err(TypeError::CannotConstrainClass {
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
