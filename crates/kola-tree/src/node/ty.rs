use derive_more::From;
use kola_macros::Inspector;
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, ops::Deref};

use kola_print::prelude::*;
use kola_utils::{as_variant, interner::StrKey};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeError;

impl<'a> Notate<'a> for NodePrinter<'a, TypeError> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        "TypeError".red().display_in(arena)
    }
}

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct QualifiedType {
    pub path: Option<Id<ModulePath>>,
    pub ty: Id<TypeName>,
}

impl<'a> Notate<'a> for NodePrinter<'a, QualifiedType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let QualifiedType { path, ty } = *self.value;

        let head = "QualifiedType".cyan().display_in(arena);

        let path = path.map(|p| self.to(p).notate(arena));
        let ty = self.to(ty).notate(arena);

        // TODO fix newlines/spacings if path is None

        let single = if let Some(path) = path.clone() {
            [arena.just(' '), path, arena.just(' '), ty.clone()].concat_in(arena)
        } else {
            [arena.just(' '), ty.clone()].concat_in(arena)
        }
        .flatten(arena);

        let multi = if let Some(path) = path {
            [arena.newline(), path, arena.newline(), ty].concat_in(arena)
        } else {
            [arena.newline(), ty].concat_in(arena)
        }
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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
        let head = "TypeVar".magenta().display_in(arena);
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
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct RecordFieldType {
    pub name: Id<ValueName>,
    pub ty: Id<Type>,
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordFieldType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordFieldType { name, ty } = *self.value;

        let head = "RecordFieldType".blue().display_in(arena);

        let name = self.to_id(name).notate(arena);
        let ty = self.to_id(ty).notate(arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", type = "),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("type = "),
            ty,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct VariantTagType {
    pub name: Id<ValueName>, // These are data constructors, therefore ValueName is used
    pub ty: Option<Id<Type>>,
}

impl<'a> Notate<'a> for NodePrinter<'a, VariantTagType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let VariantTagType { name, ty } = *self.value;

        let head = "VariantCaseType".blue().display_in(arena);

        let name = self.to_id(name).notate(arena);
        let ty = ty.map(|ty| self.to_id(ty).notate(arena));

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            ty.clone()
                .map(|ty| arena.notate(", type = ").then(ty, arena))
                .or_not(arena)
                .flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            ty.map(|ty| [arena.newline(), arena.notate("type = "), ty].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct FuncType {
    pub input: Id<Type>,
    pub output: Id<Type>,
}

impl<'a> Notate<'a> for NodePrinter<'a, FuncType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let FuncType { input, output } = *self.value;

        let head = "FuncType".blue().display_in(arena);

        let input = self.to_id(input).notate(arena);
        let output = self.to_id(output).notate(arena);

        let single = [
            arena.notate(" input = "),
            input.clone().flatten(arena),
            arena.notate(", output = "),
            output.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("input = "),
            input,
            arena.newline(),
            arena.notate("output = "),
            output,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct TypeApplication {
    pub constructor: Id<Type>,
    pub arg: Id<Type>,
}

impl<'a> Notate<'a> for NodePrinter<'a, TypeApplication> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let TypeApplication { constructor, arg } = *self.value;

        let head = "TypeApplication".blue().display_in(arena);

        let constructor = self.to_id(constructor).notate(arena);
        let arg = self.to_id(arg).notate(arena);

        let single = [
            arena.notate(" constructor = "),
            constructor.clone().flatten(arena),
            arena.notate(", arg = "),
            arg.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("constructor = "),
            constructor,
            arena.newline(),
            arena.notate("arg = "),
            arg,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug,
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

impl Type {
    #[inline]
    pub fn to_error(self) -> Option<Id<TypeError>> {
        as_variant!(self, Self::Error)
    }

    #[inline]
    pub fn to_qualified_type(self) -> Option<Id<QualifiedType>> {
        as_variant!(self, Self::Qualified)
    }

    #[inline]
    pub fn to_record_type(self) -> Option<Id<RecordType>> {
        as_variant!(self, Self::Record)
    }

    #[inline]
    pub fn to_variant_type(self) -> Option<Id<VariantType>> {
        as_variant!(self, Self::Variant)
    }

    #[inline]
    pub fn to_func_type(self) -> Option<Id<FuncType>> {
        as_variant!(self, Self::Func)
    }

    #[inline]
    pub fn to_type_application(self) -> Option<Id<TypeApplication>> {
        as_variant!(self, Self::Application)
    }

    #[inline]
    pub fn is_error(self) -> bool {
        matches!(self, Self::Error(_))
    }

    #[inline]
    pub fn is_qualified_type(self) -> bool {
        matches!(self, Self::Qualified(_))
    }

    #[inline]
    pub fn is_record_type(self) -> bool {
        matches!(self, Self::Record(_))
    }

    #[inline]
    pub fn is_variant_type(self) -> bool {
        matches!(self, Self::Variant(_))
    }

    #[inline]
    pub fn is_func_type(self) -> bool {
        matches!(self, Self::Func(_))
    }

    #[inline]
    pub fn is_type_application(self) -> bool {
        matches!(self, Self::Application(_))
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
