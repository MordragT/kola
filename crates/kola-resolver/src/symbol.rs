use derive_more::From;
use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use kola_collections::DependencyGraph;
use kola_span::SourceId;
use kola_tree::node::{
    ModuleNamespace, ModuleTypeNamespace, NamespaceKind, TypeNamespace, ValueNamespace,
};
use std::{
    collections::HashMap,
    fmt,
    marker::PhantomData,
    num::NonZeroU32,
    sync::atomic::{AtomicU32, Ordering},
};

pub type ModuleTypeGraph = DependencyGraph<ModuleTypeSym>;
pub type ModuleGraph = DependencyGraph<ModuleSym>;
pub type TypeGraph = DependencyGraph<TypeSym>;
pub type ValueGraph = DependencyGraph<ValueSym>;

pub type ModuleTypeOrders = IndexMap<ModuleSym, Vec<ModuleTypeSym>>;
pub type TypeOrders = IndexMap<ModuleSym, Vec<TypeSym>>;
pub type ValueOrders = IndexMap<ModuleSym, Vec<ValueSym>>;
pub type ModuleOrder = Vec<ModuleSym>;

pub type FileMap = IndexMap<SourceId, ModuleSym>;

static GENERATOR: AtomicU32 = AtomicU32::new(1);

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Sym<T: ?Sized> {
    id: NonZeroU32,
    t: std::marker::PhantomData<T>,
}

impl<T: ?Sized> Clone for Sym<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            t: std::marker::PhantomData,
        }
    }
}

impl<T: ?Sized> Copy for Sym<T> {}

impl<T: ?Sized> PartialEq for Sym<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T: ?Sized> Eq for Sym<T> {}

impl<T: ?Sized> PartialOrd for Sym<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for Sym<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T: ?Sized> std::hash::Hash for Sym<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T: ?Sized> std::fmt::Debug for Sym<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", stringify!(Sym), self.id)
    }
}

impl<T: ?Sized> Sym<T> {
    pub fn new() -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
        Self {
            id: NonZeroU32::new(id).expect("Sym generator overflowed"),
            t: PhantomData,
        }
    }

    pub const fn as_usize(&self) -> usize {
        self.id.get() as usize
    }

    pub const fn id(&self) -> u32 {
        self.id.get()
    }
}

impl<T: ?Sized> Default for Sym<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub type ModuleTypeSym = Sym<ModuleTypeNamespace>;
pub type ModuleSym = Sym<ModuleNamespace>;
pub type TypeSym = Sym<TypeNamespace>;
pub type ValueSym = Sym<ValueNamespace>;

impl fmt::Display for ModuleTypeSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "mt{}", self.id())
    }
}

impl fmt::Display for ModuleSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "m{}", self.id())
    }
}

impl fmt::Display for TypeSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t{}", self.id())
    }
}

impl fmt::Display for ValueSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.id())
    }
}

#[derive(Debug, EnumAsInner, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnySym {
    ModuleType(ModuleTypeSym),
    Module(ModuleSym),
    Type(TypeSym),
    Value(ValueSym),
}

impl AnySym {
    pub const fn id(&self) -> u32 {
        match self {
            Self::ModuleType(symbol) => symbol.id(),
            Self::Module(symbol) => symbol.id(),
            Self::Type(symbol) => symbol.id(),
            Self::Value(symbol) => symbol.id(),
        }
    }

    pub const fn as_usize(&self) -> usize {
        self.id() as usize
    }

    pub const fn kind(&self) -> NamespaceKind {
        match self {
            Self::ModuleType(_) => NamespaceKind::ModuleType,
            Self::Module(_) => NamespaceKind::Module,
            Self::Type(_) => NamespaceKind::Type,
            Self::Value(_) => NamespaceKind::Value,
        }
    }
}

impl fmt::Display for AnySym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ModuleType(symbol) => write!(f, "{}", symbol),
            Self::Module(symbol) => write!(f, "{}", symbol),
            Self::Type(symbol) => write!(f, "{}", symbol),
            Self::Value(symbol) => write!(f, "{}", symbol),
        }
    }
}

pub type Substitution = HashMap<AnySym, AnySym>;
