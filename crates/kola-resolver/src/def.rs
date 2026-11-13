use std::{
    collections::{HashMap, hash_map},
    hash::Hash,
    ops::Index,
};

use derive_more::From;
use kola_span::{Loc, Located};
use kola_tree::{
    node::{
        self, ModuleNamespace, ModuleTypeNamespace, Namespace, NamespaceKind, TypeNamespace,
        ValueNamespace,
    },
    prelude::Id,
};

use crate::symbol::{
    AnySym, ModuleSym, ModuleTypeSym, Substitution, Sym, TypeSym, ValueSym,
};

pub type ModuleTypeDef = Located<Id<node::ModuleTypeBind>>;
pub type ModuleDef = Located<Id<node::ModuleBind>>;
pub type TypeDef = Located<Id<node::TypeBind>>;
pub type ValueDef = Located<Id<node::ValueBind>>;

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnyDef {
    ModuleType(ModuleTypeDef),
    Module(ModuleDef),
    Type(TypeDef),
    Value(ValueDef),
}

impl AnyDef {
    pub const fn kind(&self) -> NamespaceKind {
        match self {
            AnyDef::ModuleType(_) => NamespaceKind::ModuleType,
            AnyDef::Module(_) => NamespaceKind::Module,
            AnyDef::Type(_) => NamespaceKind::Type,
            AnyDef::Value(_) => NamespaceKind::Value,
        }
    }

    pub const fn loc(&self) -> Loc {
        match self {
            AnyDef::ModuleType(info) => info.1,
            AnyDef::Module(info) => info.1,
            AnyDef::Type(info) => info.1,
            AnyDef::Value(info) => info.1,
        }
    }
}

#[derive(Debug)]
pub struct Defs<N: Namespace, T>(HashMap<Sym<N>, Located<Id<T>>>);

impl<N: Namespace, T> Defs<N, T> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert(&mut self, symbol: Sym<N>, id: Id<T>, loc: Loc) {
        self.0.insert(symbol, (id, loc));
    }

    #[inline]
    pub fn get(&self, symbol: Sym<N>) -> Option<Located<Id<T>>>
    where
        Id<T>: Copy,
    {
        self.0.get(&symbol).copied()
    }

    #[inline]
    pub fn iter(&self) -> hash_map::Iter<'_, Sym<N>, Located<Id<T>>> {
        self.0.iter()
    }
}

impl<N: Namespace, T> Clone for Defs<N, T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<N: Namespace, T> Default for Defs<N, T> {
    #[inline]
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<N: Namespace, T> IntoIterator for Defs<N, T> {
    type Item = (Sym<N>, Located<Id<T>>);
    type IntoIter = hash_map::IntoIter<Sym<N>, Located<Id<T>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, N: Namespace, T> IntoIterator for &'a Defs<N, T> {
    type Item = (&'a Sym<N>, &'a Located<Id<T>>);
    type IntoIter = hash_map::Iter<'a, Sym<N>, Located<Id<T>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, N: Namespace, T> IntoIterator for &'a mut Defs<N, T> {
    type Item = (&'a Sym<N>, &'a mut Located<Id<T>>);
    type IntoIter = hash_map::IterMut<'a, Sym<N>, Located<Id<T>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<N: Namespace, T> Index<Sym<N>> for Defs<N, T> {
    type Output = Located<Id<T>>;

    fn index(&self, sym: Sym<N>) -> &Self::Output {
        self.0.get(&sym).expect("Bind not found")
    }
}





#[derive(Debug, Clone, Default)]
pub struct DefMap {
    module_types: Defs<ModuleTypeNamespace, node::ModuleTypeBind>,
    modules: Defs<ModuleNamespace, node::ModuleBind>,
    types: Defs<TypeNamespace, node::TypeBind>,
    values: Defs<ValueNamespace, node::ValueBind>,
}

impl DefMap {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert_module_type(
        &mut self,
        sym: ModuleTypeSym,
        id: Id<node::ModuleTypeBind>,
        loc: Loc,
    ) {
        self.module_types.insert(sym, id, loc);
    }

    #[inline]
    pub fn insert_module(&mut self, sym: ModuleSym, id: Id<node::ModuleBind>, loc: Loc) {
        self.modules.insert(sym, id, loc);
    }

    #[inline]
    pub fn insert_type(&mut self, sym: TypeSym, id: Id<node::TypeBind>, loc: Loc) {
        self.types.insert(sym, id, loc);
    }

    #[inline]
    pub fn insert_value(&mut self, sym: ValueSym, id: Id<node::ValueBind>, loc: Loc) {
        self.values.insert(sym, id, loc);
    }

    #[inline]
    pub fn get_module_type(&self, sym: ModuleTypeSym) -> Option<ModuleTypeDef> {
        self.module_types.get(sym)
    }

    #[inline]
    pub fn get_module(&self, sym: ModuleSym) -> Option<ModuleDef> {
        self.modules.get(sym)
    }

    #[inline]
    pub fn get_type(&self, sym: TypeSym) -> Option<TypeDef> {
        self.types.get(sym)
    }

    #[inline]
    pub fn get_value(&self, sym: ValueSym) -> Option<ValueDef> {
        self.values.get(sym)
    }

    #[inline]
    pub fn get(&self, sym: impl Into<AnySym>) -> Option<AnyDef> {
        match sym.into() {
            AnySym::ModuleType(sym) => self.get_module_type(sym).map(AnyDef::ModuleType),
            AnySym::Module(sym) => self.get_module(sym).map(AnyDef::Module),
            AnySym::Type(sym) => self.get_type(sym).map(AnyDef::Type),
            AnySym::Value(sym) => self.get_value(sym).map(AnyDef::Value),
        }
    }

    #[inline]
    pub fn iter_module_types(&self) -> impl Iterator<Item = (ModuleTypeSym, ModuleTypeDef)> {
        self.module_types.iter().map(|(&sym, &def)| (sym, def))
    }

    #[inline]
    pub fn iter_modules(&self) -> impl Iterator<Item = (ModuleSym, ModuleDef)> {
        self.modules.iter().map(|(&sym, &def)| (sym, def))
    }

    #[inline]
    pub fn iter_types(&self) -> impl Iterator<Item = (TypeSym, TypeDef)> {
        self.types.iter().map(|(&sym, &def)| (sym, def))
    }

    #[inline]
    pub fn iter_values(&self) -> impl Iterator<Item = (ValueSym, ValueDef)> {
        self.values.iter().map(|(&sym, &def)| (sym, def))
    }
}

impl Index<ModuleTypeSym> for DefMap {
    type Output = ModuleTypeDef;

    fn index(&self, index: ModuleTypeSym) -> &Self::Output {
        &self.module_types[index]
    }
}

impl Index<ModuleSym> for DefMap {
    type Output = ModuleDef;

    fn index(&self, index: ModuleSym) -> &Self::Output {
        &self.modules[index]
    }
}

impl Index<TypeSym> for DefMap {
    type Output = TypeDef;

    fn index(&self, index: TypeSym) -> &Self::Output {
        &self.types[index]
    }
}

impl Index<ValueSym> for DefMap {
    type Output = ValueDef;

    fn index(&self, index: ValueSym) -> &Self::Output {
        &self.values[index]
    }
}

