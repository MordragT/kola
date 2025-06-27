use std::{fmt, hash::Hash, ops::Index};

use derive_more::From;
use kola_collections::{HashMap, hash_map};
use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{
        self, ModuleNamespace, ModuleTypeNamespace, Namespace, NamespaceKind, TypeNamespace,
        ValueNamespace, Vis,
    },
};

use crate::symbol::{AnySym, ModuleSym, ModuleTypeSym, Sym, TypeSym, ValueSym};

pub struct Def<T> {
    pub loc: Loc,
    pub vis: Vis,
    pub id: Option<Id<T>>,
}

impl<T> Def<T> {
    pub const fn bound(id: Id<T>, vis: Vis, loc: Loc) -> Self {
        Self {
            loc,
            vis,
            id: Some(id),
        }
    }

    pub const fn unbound(loc: Loc, vis: Vis) -> Self {
        Self { loc, vis, id: None }
    }

    pub const fn loc(&self) -> Loc {
        self.loc
    }

    pub const fn vis(&self) -> Vis {
        self.vis
    }

    pub fn id(&self) -> Id<T> {
        self.id.unwrap()
    }
}

impl<T> fmt::Debug for Def<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BindInfo")
            .field("id", &self.id)
            .field("loc", &self.loc)
            .field("vis", &self.vis)
            .finish()
    }
}

impl<T> Clone for Def<T> {
    fn clone(&self) -> Self {
        Self {
            loc: self.loc,
            vis: self.vis,
            id: self.id,
        }
    }
}

impl<T> Copy for Def<T> {}

impl<T> PartialEq for Def<T> {
    fn eq(&self, other: &Self) -> bool {
        self.loc == other.loc
    }
}

impl<T> Eq for Def<T> {}

impl<T> PartialOrd for Def<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Def<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.loc.cmp(&other.loc)
    }
}

impl<T> Hash for Def<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.loc.hash(state);
    }
}

pub type ModuleTypeDef = Def<node::ModuleTypeBind>;
pub type ModuleDef = Def<node::ModuleBind>;
pub type TypeDef = Def<node::TypeBind>;
pub type ValueDef = Def<node::ValueBind>;

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

    pub const fn location(&self) -> Loc {
        match self {
            AnyDef::ModuleType(info) => info.loc,
            AnyDef::Module(info) => info.loc,
            AnyDef::Type(info) => info.loc,
            AnyDef::Value(info) => info.loc,
        }
    }

    pub const fn visibility(&self) -> Vis {
        match self {
            AnyDef::ModuleType(info) => info.vis,
            AnyDef::Module(info) => info.vis,
            AnyDef::Type(info) => info.vis,
            AnyDef::Value(info) => info.vis,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Defs<N: Namespace, T>(HashMap<Sym<N>, Def<T>>);

impl<N: Namespace, T> Defs<N, T> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert(&mut self, symbol: Sym<N>, info: Def<T>) {
        self.0.insert(symbol, info);
    }

    #[inline]
    pub fn get(&self, symbol: Sym<N>) -> Option<Def<T>>
    where
        Def<T>: Copy,
    {
        self.0.get(&symbol).copied()
    }

    #[inline]
    pub fn iter(&self) -> hash_map::Iter<Sym<N>, Def<T>> {
        self.0.iter()
    }
}

impl<N: Namespace, T> Default for Defs<N, T> {
    #[inline]
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<N: Namespace, T> IntoIterator for Defs<N, T> {
    type Item = (Sym<N>, Def<T>);
    type IntoIter = hash_map::IntoIter<Sym<N>, Def<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, N: Namespace, T> IntoIterator for &'a Defs<N, T> {
    type Item = (&'a Sym<N>, &'a Def<T>);
    type IntoIter = hash_map::Iter<'a, Sym<N>, Def<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, N: Namespace, T> IntoIterator for &'a mut Defs<N, T> {
    type Item = (&'a Sym<N>, &'a mut Def<T>);
    type IntoIter = hash_map::IterMut<'a, Sym<N>, Def<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<N: Namespace, T> Index<Sym<N>> for Defs<N, T> {
    type Output = Def<T>;

    fn index(&self, sym: Sym<N>) -> &Self::Output {
        self.0.get(&sym).expect("Bind not found")
    }
}

#[derive(Debug, Clone, Default)]
pub struct Definitions {
    module_types: Defs<ModuleTypeNamespace, node::ModuleTypeBind>,
    modules: Defs<ModuleNamespace, node::ModuleBind>,
    types: Defs<TypeNamespace, node::TypeBind>,
    values: Defs<ValueNamespace, node::ValueBind>,
}

impl Definitions {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert_module_type(&mut self, sym: ModuleTypeSym, def: ModuleTypeDef) {
        self.module_types.insert(sym, def);
    }

    #[inline]
    pub fn insert_module(&mut self, sym: ModuleSym, def: ModuleDef) {
        self.modules.insert(sym, def);
    }

    #[inline]
    pub fn insert_type(&mut self, sym: TypeSym, def: TypeDef) {
        self.types.insert(sym, def);
    }

    #[inline]
    pub fn insert_value(&mut self, sym: ValueSym, def: ValueDef) {
        self.values.insert(sym, def);
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
            AnySym::Value(sym) => self.get_value(sym).map(AnyDef::Value),
            AnySym::Type(sym) => self.get_type(sym).map(AnyDef::Type),
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

impl Index<ModuleTypeSym> for Definitions {
    type Output = ModuleTypeDef;

    fn index(&self, index: ModuleTypeSym) -> &Self::Output {
        &self.module_types[index]
    }
}

impl Index<ModuleSym> for Definitions {
    type Output = ModuleDef;

    fn index(&self, index: ModuleSym) -> &Self::Output {
        &self.modules[index]
    }
}

impl Index<ValueSym> for Definitions {
    type Output = ValueDef;

    fn index(&self, index: ValueSym) -> &Self::Output {
        &self.values[index]
    }
}

impl Index<TypeSym> for Definitions {
    type Output = TypeDef;

    fn index(&self, index: TypeSym) -> &Self::Output {
        &self.types[index]
    }
}
