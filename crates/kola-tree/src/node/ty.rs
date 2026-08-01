use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_tree_macro::Inspector;
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, ops::Deref};

use kola_print::prelude::*;
use kola_utils::interner::StrKey;

use super::{KindName, ModulePath, TypeName, ValueName};

use crate::{id::Id, print::TreePrinter, slice::SliceId, tree::TreeView};

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
#[notate(with = TreePrinter<'a>, color = "green")]
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
#[notate(with = TreePrinter<'a>, color = "green")]
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
#[notate(with = TreePrinter<'a>, color = "cyan")]
pub struct CompType {
    pub ty: Id<TypeExpr>,
    pub effect: Option<Id<EffectType>>,
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "red")]
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
#[notate(with = TreePrinter<'a>, color = "cyan")]
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

impl<'a> Notate<'a, TypeVar> for TreePrinter<'a> {
    fn notate(&self, value: &TypeVar, arena: &'a Bump) -> Notation<'a> {
        let head = "TypeVar".cyan().display_in(arena);
        let value = self
            .interner
            .get(value.0)
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
#[notate(with = TreePrinter<'a>, color = "cyan")]
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
#[notate(with = TreePrinter<'a>, color = "cyan")]
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct RecordType {
    pub fields: SliceId<RecordFieldType>,
    pub extension: Option<Id<TypeName>>,
}

impl RecordType {
    pub fn get(&self, index: usize, storage: &impl TreeView) -> RecordFieldType {
        *self.fields.iter(storage).nth(index).unwrap().get(storage)
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
#[notate(with = TreePrinter<'a>, color = "cyan")]
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct VariantType {
    pub tags: SliceId<TagType>,
    pub extension: Option<Id<TypeName>>,
}

impl VariantType {
    pub fn get(&self, index: usize, storage: &impl TreeView) -> TagType {
        *self.tags.iter(storage).nth(index).unwrap().get(storage)
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
#[notate(with = TreePrinter<'a>, color = "cyan")]
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
#[notate(with = TreePrinter<'a>, color = "cyan")]
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

impl<'a> Notate<'a, TypeExpr> for TreePrinter<'a> {
    fn notate(&self, value: &TypeExpr, arena: &'a Bump) -> Notation<'a> {
        match value {
            TypeExpr::Error(e) => self.notate(e, arena),
            TypeExpr::Qualified(p) => self.notate(p, arena),
            TypeExpr::Record(r) => self.notate(r, arena),
            TypeExpr::Variant(v) => self.notate(v, arena),
            TypeExpr::Func(f) => self.notate(f, arena),
            TypeExpr::Application(a) => self.notate(a, arena),
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
#[notate(with = TreePrinter<'a>, color = "cyan")]
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
#[notate(with = TreePrinter<'a>, color = "cyan")]
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
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct TypeScheme {
    pub forall: Option<Id<ForallBinder>>,
    pub ty: Id<TypeExpr>,
}
