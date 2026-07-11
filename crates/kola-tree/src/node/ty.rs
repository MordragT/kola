use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_macros::{Inspector, Notate};
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, ops::Deref};

use kola_print::prelude::*;
use kola_utils::interner::StrKey;

use super::{KindName, ModulePath, NodeStorage, TypeName, ValueName};

use crate::{
    id::{Id, SliceId},
    print::NodePrinter,
};

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "green")]
pub struct EffectOpType {
    pub name: Id<ValueName>,
    pub ty: Id<TypeExpr>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "green")]
pub struct EffectType(pub SliceId<EffectOpType>);

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
pub struct CompType {
    pub ty: Id<TypeExpr>,
    pub effect: Option<Id<EffectType>>,
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "red")]
pub struct TypeError;

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
pub struct QualifiedType {
    pub path: Option<Id<ModulePath>>,
    pub ty: Id<TypeName>, // TODO This also includes type variables which is a bit surprising
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct TypeVar(pub StrKey);

impl TypeVar {
    #[inline]
    pub fn as_str_key(&self) -> &StrKey {
        &self.0
    }
}

impl Deref for TypeVar {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<StrKey> for TypeVar {
    #[inline]
    fn as_ref(&self) -> &StrKey {
        &self.0
    }
}

impl Borrow<StrKey> for TypeVar {
    #[inline]
    fn borrow(&self) -> &StrKey {
        &self.0
    }
}

impl PartialEq<StrKey> for TypeVar {
    #[inline]
    fn eq(&self, other: &StrKey) -> bool {
        self == other
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, TypeVar> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "TypeVar".cyan().display_in(arena);
        let value = self
            .interner
            .get(self.value.0)
            .expect("Symbol not found")
            .magenta()
            .display_in(arena);

        let single = arena.just(' ').then(value.clone(), arena);
        let multi = arena.newline().then(value, arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
pub enum LabelOrVar {
    Var(Id<TypeVar>),
    Label(Id<ValueName>),
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
pub struct RecordFieldType {
    pub label_or_var: Id<LabelOrVar>,
    pub ty: Id<TypeExpr>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct RecordType {
    pub fields: SliceId<RecordFieldType>,
    pub extension: Option<Id<TypeName>>,
}

impl RecordType {
    pub fn get(&self, index: usize, arena: &NodeStorage) -> RecordFieldType {
        *self.fields.iter(arena).nth(index).unwrap().get(arena)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
pub struct TagType {
    pub name: Id<ValueName>, // These are data constructors, therefore ValueName is used
    pub ty: Option<Id<TypeExpr>>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
pub struct VariantType {
    pub tags: SliceId<TagType>,
    pub extension: Option<Id<TypeName>>,
}

impl VariantType {
    pub fn get(&self, index: usize, arena: &NodeStorage) -> TagType {
        *self.tags.iter(arena).nth(index).unwrap().get(arena)
    }
}

// TODO this needs to be disambiguated with parentheses if a function should be one argument
#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
pub struct FuncType {
    pub input: Id<TypeExpr>,
    pub output: Id<CompType>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
pub struct TypeApplication {
    pub constructor: Id<TypeExpr>,
    pub arg: Id<TypeExpr>,
}

#[derive(
    Debug,
    EnumAsInner,
    Inspector,
    From,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum TypeExpr {
    Error(Id<TypeError>),
    Qualified(Id<QualifiedType>),
    // TODO put a TypeVar here as variant
    Record(Id<RecordType>),
    Variant(Id<VariantType>),
    Func(Id<FuncType>),
    Application(Id<TypeApplication>),
}

impl<'a> Notate<'a> for NodePrinter<'a, TypeExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            TypeExpr::Error(e) => self.to(e).notate(arena),
            TypeExpr::Qualified(p) => self.to(p).notate(arena),
            TypeExpr::Record(r) => self.to(r).notate(arena),
            TypeExpr::Variant(v) => self.to(v).notate(arena),
            TypeExpr::Func(f) => self.to(f).notate(arena),
            TypeExpr::Application(a) => self.to(a).notate(arena),
        }
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
pub struct TypeVarBind {
    pub kind: Option<Id<KindName>>,
    pub var: Id<TypeVar>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
pub struct ForallBinder(pub SliceId<TypeVarBind>);

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "green")]
pub struct TypeScheme {
    pub forall: Option<Id<ForallBinder>>,
    pub ty: Id<TypeExpr>,
}
