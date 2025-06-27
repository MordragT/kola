use derive_more::{Display, From};
use enum_as_inner::EnumAsInner;
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, marker::PhantomData, ops::Deref};

use kola_print::prelude::*;
use kola_utils::interner::StrKey;

use crate::print::NodePrinter;

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum NamespaceKind {
    ModuleType,
    Module,
    Type,
    Value,
}

mod sealed {
    pub trait Sealed {}

    impl Sealed for super::ModuleTypeNamespace {}
    impl Sealed for super::ModuleNamespace {}
    impl Sealed for super::TypeNamespace {}
    impl Sealed for super::ValueNamespace {}
}

pub trait Namespace: sealed::Sealed {
    const KIND: NamespaceKind;
}

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ModuleTypeNamespace;

impl Namespace for ModuleTypeNamespace {
    const KIND: NamespaceKind = NamespaceKind::ModuleType;
}

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ModuleNamespace;

impl Namespace for ModuleNamespace {
    const KIND: NamespaceKind = NamespaceKind::Module;
}

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct TypeNamespace;

impl Namespace for TypeNamespace {
    const KIND: NamespaceKind = NamespaceKind::Type;
}

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ValueNamespace;

impl Namespace for ValueNamespace {
    const KIND: NamespaceKind = NamespaceKind::Value;
}

pub type ModuleTypeName = Name<ModuleTypeNamespace>;
pub type ModuleName = Name<ModuleNamespace>;
pub type TypeName = Name<TypeNamespace>;
pub type ValueName = Name<ValueNamespace>;

#[derive(Debug, Serialize, Deserialize)]
pub struct Name<N>(pub StrKey, PhantomData<N>);

impl<N> Name<N> {
    #[inline]
    pub fn new(key: StrKey) -> Self {
        Name(key, PhantomData)
    }

    #[inline]
    pub fn as_str_key(&self) -> &StrKey {
        &self.0
    }

    #[inline]
    pub fn kind() -> NamespaceKind
    where
        N: Namespace,
    {
        N::KIND
    }
}

// impl<N> std::fmt::Debug for Name<N> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.debug_tuple("Name").field(&self.0).finish()
//     }
// }

impl<N> Clone for Name<N> {
    fn clone(&self) -> Self {
        Name(self.0, PhantomData)
    }
}

impl<N> Copy for Name<N> {}

impl<N> PartialEq for Name<N> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<N> Eq for Name<N> {}

impl<N> PartialOrd for Name<N> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<N> Ord for Name<N> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<N> std::hash::Hash for Name<N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<N> Deref for Name<N> {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<N> AsRef<StrKey> for Name<N> {
    #[inline]
    fn as_ref(&self) -> &StrKey {
        &self.0
    }
}

impl<N> Borrow<StrKey> for Name<N> {
    #[inline]
    fn borrow(&self) -> &StrKey {
        &self.0
    }
}

impl<N> PartialEq<StrKey> for Name<N> {
    #[inline]
    fn eq(&self, other: &StrKey) -> bool {
        &self.0 == other
    }
}

impl From<StrKey> for ModuleName {
    #[inline]
    fn from(key: StrKey) -> Self {
        ModuleName::new(key)
    }
}

impl From<StrKey> for TypeName {
    #[inline]
    fn from(key: StrKey) -> Self {
        TypeName::new(key)
    }
}

impl From<StrKey> for ValueName {
    #[inline]
    fn from(key: StrKey) -> Self {
        ValueName::new(key)
    }
}

impl<'a, N> Notate<'a> for NodePrinter<'a, Name<N>> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "Name".cyan().display_in(arena);

        let name = self
            .interner
            .get(self.value.0)
            .unwrap()
            .yellow()
            .display_in(arena)
            .enclose_by(arena.just('"'), arena);

        let single = [arena.just(' '), name.clone()].concat_in(arena);
        let multi = [arena.newline(), name].concat_in(arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug,
    EnumAsInner,
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
pub enum AnyName {
    ModuleType(ModuleTypeName),
    Module(ModuleName),
    Type(TypeName),
    Value(ValueName),
}

impl AnyName {
    pub fn kind(&self) -> NamespaceKind {
        match self {
            AnyName::ModuleType(_) => NamespaceKind::ModuleType,
            AnyName::Module(_) => NamespaceKind::Module,
            AnyName::Type(_) => NamespaceKind::Type,
            AnyName::Value(_) => NamespaceKind::Value,
        }
    }

    pub fn as_str_key(&self) -> &StrKey {
        match self {
            AnyName::ModuleType(name) => name.as_str_key(),
            AnyName::Module(name) => name.as_str_key(),
            AnyName::Type(name) => name.as_str_key(),
            AnyName::Value(name) => name.as_str_key(),
        }
    }
}
