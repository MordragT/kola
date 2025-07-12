use derive_more::{Display, From, IntoIterator};
use enum_as_inner::EnumAsInner;
use kola_macros::{Inspector, Notate};
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, fmt, ops::Deref};

use kola_print::prelude::*;
use kola_utils::interner::StrKey;

use crate::{
    id::Id,
    node::{EffectName, ModulePath, TypeName, ValueName},
    print::NodePrinter,
    tree::TreeView,
};

/*
type Option = forall a . [ Some : a, None ]
type OptionResult  = forall a e . [ Option a | +Error : e ]
type AlwaysSome = forall a . [ Option a | -None ]

type Person = { name : Str }
type Member = { Person | +id : Num }
type Id = { Member | -id }

map : forall a b . (a -> b) -> List a -> List b

TODO:

allow Open Variants and Open Records:

< Some : a, None | * >
{ name : Str, age : Num | * }

as well as (or just allow the latter, as it doesn't need any special handling)

forall a b . < Some : a, None | b >
forall a . { name : Str, age : Num | b }
*/

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
pub struct QualifiedEffectType {
    pub path: Option<Id<ModulePath>>,
    pub ty: Id<EffectName>,
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
#[notate(color = "green")]
pub struct EffectOpType {
    pub name: Id<ValueName>,
    pub ty: Id<Type>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    IntoIterator,
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
#[into_iterator(owned, ref)]
pub struct EffectRowType(pub Vec<Id<EffectOpType>>);

// #[derive(
//     Debug,
//     Notate,
//     Inspector,
//     Clone,
//     Copy,
//     PartialEq,
//     Eq,
//     PartialOrd,
//     Ord,
//     Hash,
//     Serialize,
//     Deserialize,
// )]
// #[notate(color = "green")]
// pub struct EffectOpTypeScheme {
//     pub name: Id<ValueName>,
//     pub ty_scheme: Id<TypeScheme>,
// }

// #[derive(
//     Debug,
//     Notate,
//     Inspector,
//     From,
//     IntoIterator,
//     Clone,
//     PartialEq,
//     Eq,
//     PartialOrd,
//     Ord,
//     Hash,
//     Serialize,
//     Deserialize,
// )]
// #[notate(color = "green")]
// #[into_iterator(owned, ref)]
// pub struct EffectRowTypeScheme(pub Vec<Id<EffectOpTypeScheme>>);

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum EffectType {
    // ~ Io (reference to bound effect type)
    Qualified(Id<QualifiedEffectType>),
    // ~ { print : Str -> Unit }
    Row(Id<EffectRowType>),
}

impl<'a> Notate<'a> for NodePrinter<'a, EffectType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            EffectType::Qualified(q) => self.to(q).notate(arena),
            EffectType::Row(r) => self.to(r).notate(arena),
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
pub struct CompType {
    pub ty: Id<Type>,
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
pub enum Label {
    // TODO rename to LabelOrVar
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
    pub label_or_var: Id<Label>,
    pub ty: Id<Type>,
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
    pub fields: Vec<Id<RecordFieldType>>,
    pub extension: Option<Id<TypeName>>,
}

impl RecordType {
    pub fn get(&self, index: usize, tree: &impl TreeView) -> RecordFieldType {
        *self.fields[index].get(tree)
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
    pub ty: Option<Id<Type>>,
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
    pub tags: Vec<Id<TagType>>,
    pub extension: Option<Id<TypeName>>,
}

impl VariantType {
    pub fn get(&self, index: usize, tree: &impl TreeView) -> TagType {
        *self.tags[index].get(tree)
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
    pub input: Id<Type>,
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
    pub constructor: Id<Type>,
    pub arg: Id<Type>,
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
pub enum Type {
    Error(Id<TypeError>),
    Qualified(Id<QualifiedType>),
    // TODO put a TypeVar here as variant
    Record(Id<RecordType>),
    Variant(Id<VariantType>),
    Func(Id<FuncType>),
    Application(Id<TypeApplication>),
}

impl<'a> Notate<'a> for NodePrinter<'a, Type> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Type::Error(e) => self.to(e).notate(arena),
            Type::Qualified(p) => self.to(p).notate(arena),
            Type::Record(r) => self.to(r).notate(arena),
            Type::Variant(v) => self.to(v).notate(arena),
            Type::Func(f) => self.to(f).notate(arena),
            Type::Application(a) => self.to(a).notate(arena),
        }
    }
}

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Kind {
    // Type,
    Record,
    Label,
}

impl<'a> Notate<'a> for NodePrinter<'a, Kind> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        self.value.cyan().display_in(arena)
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
    pub kind: Option<Id<Kind>>,
    pub var: Id<TypeVar>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    IntoIterator,
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
#[into_iterator(owned, ref)]
pub struct ForallBinder(pub Vec<Id<TypeVarBind>>);

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
    pub ty: Id<Type>,
}
