use std::{fmt, hash::Hash, marker::PhantomData, ops::Index};

use derive_more::From;
use kola_collections::{HashMap, hash_map};
use kola_span::Loc;
use kola_tree::node::{
    ModuleNamespace, Namespace, NamespaceKind, TypeNamespace, ValueNamespace, Vis,
};

use crate::symbol::Sym;

pub struct Def<T> {
    pub loc: Loc,
    pub vis: Vis,
    pub t: PhantomData<T>,
}

impl<T> Def<T> {
    pub const fn new(loc: Loc, vis: Vis) -> Self {
        Self {
            loc,
            vis,
            t: PhantomData,
        }
    }

    pub const fn loc(&self) -> Loc {
        self.loc
    }

    pub const fn vis(&self) -> Vis {
        self.vis
    }
}

impl<T> fmt::Debug for Def<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BindInfo")
            // .field("id", &self.id)
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
            t: PhantomData,
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

pub type ModuleDef = Def<ModuleNamespace>;
pub type TypeDef = Def<TypeNamespace>;
pub type ValueDef = Def<ValueNamespace>;

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnyDef {
    Module(ModuleDef),
    Type(TypeDef),
    Value(ValueDef),
}

impl AnyDef {
    pub const fn kind(&self) -> NamespaceKind {
        match self {
            AnyDef::Module(_) => NamespaceKind::Module,
            AnyDef::Type(_) => NamespaceKind::Type,
            AnyDef::Value(_) => NamespaceKind::Value,
        }
    }

    pub const fn location(&self) -> Loc {
        match self {
            AnyDef::Module(info) => info.loc,
            AnyDef::Type(info) => info.loc,
            AnyDef::Value(info) => info.loc,
        }
    }

    pub const fn visibility(&self) -> Vis {
        match self {
            AnyDef::Module(info) => info.vis,
            AnyDef::Type(info) => info.vis,
            AnyDef::Value(info) => info.vis,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Defs<N: Namespace>(HashMap<Sym<N>, Def<N>>);

impl<N: Namespace> Defs<N> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert(&mut self, symbol: Sym<N>, info: Def<N>) {
        self.0.insert(symbol, info);
    }

    #[inline]
    pub fn get(&self, symbol: Sym<N>) -> Option<Def<N>> {
        self.0.get(&symbol).copied()
    }

    #[inline]
    pub fn iter(&self) -> hash_map::Iter<Sym<N>, Def<N>> {
        self.0.iter()
    }
}

impl<N: Namespace> Default for Defs<N> {
    #[inline]
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<N: Namespace> IntoIterator for Defs<N> {
    type Item = (Sym<N>, Def<N>);
    type IntoIter = hash_map::IntoIter<Sym<N>, Def<N>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, N: Namespace> IntoIterator for &'a Defs<N> {
    type Item = (&'a Sym<N>, &'a Def<N>);
    type IntoIter = hash_map::Iter<'a, Sym<N>, Def<N>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, N: Namespace> IntoIterator for &'a mut Defs<N> {
    type Item = (&'a Sym<N>, &'a mut Def<N>);
    type IntoIter = hash_map::IterMut<'a, Sym<N>, Def<N>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<N: Namespace> Index<Sym<N>> for Defs<N> {
    type Output = Def<N>;

    fn index(&self, sym: Sym<N>) -> &Self::Output {
        self.0.get(&sym).expect("Bind not found")
    }
}
