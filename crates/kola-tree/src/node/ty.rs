use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_macros::{Inspector, Notate};
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, ops::Deref};

use kola_print::prelude::*;
use kola_utils::interner::StrKey;

use crate::{
    id::Id,
    node::{ModulePath, TypeName, ValueName},
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
    pub ty: Id<TypeName>,
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
pub struct RecordFieldType {
    pub name: Id<ValueName>,
    pub ty: Id<Type>,
}

#[derive(
    Debug, Inspector, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct RecordType {
    pub fields: Vec<Id<RecordFieldType>>,
    pub extension: Option<Id<TypeName>>,
}

impl RecordType {
    pub fn get(&self, index: usize, tree: &impl TreeView) -> RecordFieldType {
        *self.fields[index].get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordType { fields, extension } = self.value;

        let head = "RecordType".blue().display_in(arena);

        let fields = self.to_slice(fields).gather(arena);
        let extension = extension.map(|ext| self.to_id(ext).notate(arena));

        let single = [
            arena.notate(" fields = "),
            fields.clone().concat_map(
                |field| arena.just(' ').then(field, arena).flatten(arena),
                arena,
            ),
            extension
                .clone()
                .map(|ext| arena.notate(" extension = ").then(ext, arena))
                .or_not(arena)
                .flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("fields = "),
            fields.concat_by(arena.newline(), arena),
            extension
                .map(|ext| [arena.newline(), arena.notate("extension = "), ext].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

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
pub struct VariantTagType {
    pub name: Id<ValueName>, // These are data constructors, therefore ValueName is used
    pub ty: Option<Id<Type>>,
}

#[derive(
    Debug, Inspector, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct VariantType {
    pub cases: Vec<Id<VariantTagType>>,
    pub extension: Option<Id<TypeName>>,
}

impl VariantType {
    pub fn get(&self, index: usize, tree: &impl TreeView) -> VariantTagType {
        *self.cases[index].get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, VariantType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let VariantType { cases, extension } = self.value;

        let head = "VariantType".blue().display_in(arena);

        let cases = self.to_slice(cases).gather(arena);
        let extension = extension.map(|ext| self.to_id(ext).notate(arena));

        let single = [
            arena.notate(" cases = "),
            cases.clone().concat_map(
                |field| arena.just(' ').then(field, arena).flatten(arena),
                arena,
            ),
            extension
                .clone()
                .map(|ext| arena.notate(" extension = ").then(ext, arena))
                .or_not(arena)
                .flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("cases = "),
            cases.concat_by(arena.newline(), arena),
            extension
                .map(|ext| [arena.newline(), arena.notate("extension = "), ext].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
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
    pub output: Id<Type>,
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

#[derive(Debug, Inspector, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeScheme {
    pub vars: Vec<Id<TypeVar>>,
    pub ty: Id<Type>,
}

impl<'a> Notate<'a> for NodePrinter<'a, TypeScheme> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let TypeScheme { vars, ty } = self.value;

        let head = "TypeScheme".green().display_in(arena);

        let vars = self.to_slice(vars).gather(arena);
        let ty = self.to_id(*ty).notate(arena);

        let single = if vars.is_empty() {
            arena
                .notate(" type = ")
                .then(ty.clone(), arena)
                .flatten(arena)
        } else {
            [
                arena.notate(" vars = "),
                vars.clone()
                    .concat_by(arena.just(' '), arena)
                    .flatten(arena),
                arena.notate(", type = "),
                ty.clone().flatten(arena),
            ]
            .concat_in(arena)
        };

        let multi = if vars.is_empty() {
            [arena.newline(), arena.notate("type = "), ty]
                .concat_in(arena)
                .indent(arena)
        } else {
            [
                arena.newline(),
                arena.notate("vars = "),
                vars.concat_by(arena.newline(), arena),
                arena.newline(),
                arena.notate("type = "),
                ty,
            ]
            .concat_in(arena)
            .indent(arena)
        };

        head.then(single.or(multi, arena), arena)
    }
}
