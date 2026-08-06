use derive_more::From;
use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use kola_collections::DependencyGraph;
use kola_span::SourceId;
use kola_subst::Substitutable;
use kola_tree::node::{
    FunctorNamespace, ModuleNamespace, ModuleTypeNamespace, NamespaceKind, TypeNamespace,
    ValueNamespace,
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

static LEVEL: AtomicU32 = AtomicU32::new(1);
static GENERATOR: AtomicU32 = AtomicU32::new(1);

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Sym<T: ?Sized> {
    id: NonZeroU32,
    level: NonZeroU32,
    t: std::marker::PhantomData<T>,
}

impl<T: ?Sized> Clone for Sym<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            level: self.level,
            t: std::marker::PhantomData,
        }
    }
}

impl<T: ?Sized> Copy for Sym<T> {}

impl<T: ?Sized> PartialEq for Sym<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.level == other.level
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
        self.id.cmp(&other.id).then(self.level.cmp(&other.level))
    }
}

impl<T: ?Sized> std::hash::Hash for Sym<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.level.hash(state);
    }
}

impl<T: ?Sized> std::fmt::Debug for Sym<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({}, level: {})", stringify!(Sym), self.id, self.level)
    }
}

impl<T: ?Sized> Sym<T> {
    pub fn new() -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
        let level = Self::load_level();
        Self {
            id: NonZeroU32::new(id).expect("Sym generator overflowed"),
            level: NonZeroU32::new(level).expect("Sym level overflowed"),
            t: PhantomData,
        }
    }

    pub const fn as_usize(&self) -> usize {
        self.id.get() as usize
    }

    pub const fn id(&self) -> u32 {
        self.id.get()
    }

    pub const fn level(&self) -> u32 {
        self.level.get()
    }

    pub fn load_level() -> u32 {
        LEVEL.load(Ordering::Relaxed)
    }

    pub fn enter() {
        LEVEL.fetch_add(1, Ordering::Relaxed);
    }

    pub fn exit() {
        LEVEL.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn branch<U>(mut f: impl FnMut() -> U) -> U {
        LEVEL.fetch_add(1, Ordering::Relaxed);
        let result = f();
        LEVEL.fetch_sub(1, Ordering::Relaxed);
        result
    }
}

impl<T: ?Sized> Default for Sym<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub type FunctorSym = Sym<FunctorNamespace>;
pub type ModuleTypeSym = Sym<ModuleTypeNamespace>;
pub type ModuleSym = Sym<ModuleNamespace>;
pub type TypeSym = Sym<TypeNamespace>;
pub type ValueSym = Sym<ValueNamespace>;

impl fmt::Display for FunctorSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "f{}", self.id())
    }
}

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
    Functor(FunctorSym),
    ModuleType(ModuleTypeSym),
    Module(ModuleSym),
    Type(TypeSym),
    Value(ValueSym),
}

impl AnySym {
    pub const fn id(&self) -> u32 {
        match self {
            Self::Functor(symbol) => symbol.id(),
            Self::ModuleType(symbol) => symbol.id(),
            Self::Module(symbol) => symbol.id(),
            Self::Type(symbol) => symbol.id(),
            Self::Value(symbol) => symbol.id(),
        }
    }

    pub const fn as_usize(&self) -> usize {
        self.id() as usize
    }

    pub const fn level(&self) -> u32 {
        match self {
            Self::Functor(symbol) => symbol.level(),
            Self::ModuleType(symbol) => symbol.level(),
            Self::Module(symbol) => symbol.level(),
            Self::Type(symbol) => symbol.level(),
            Self::Value(symbol) => symbol.level(),
        }
    }

    pub const fn kind(&self) -> NamespaceKind {
        match self {
            Self::Functor(_) => NamespaceKind::Functor,
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
            Self::Functor(symbol) => write!(f, "{}", symbol),
            Self::ModuleType(symbol) => write!(f, "{}", symbol),
            Self::Module(symbol) => write!(f, "{}", symbol),
            Self::Type(symbol) => write!(f, "{}", symbol),
            Self::Value(symbol) => write!(f, "{}", symbol),
        }
    }
}

pub type Substitution = HashMap<AnySym, AnySym>;

impl Substitutable<Substitution> for FunctorSym {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::Functor(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_functor().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

impl Substitutable<Substitution> for ModuleTypeSym {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::ModuleType(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_module_type().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

impl Substitutable<Substitution> for ModuleSym {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::Module(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_module().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

impl Substitutable<Substitution> for TypeSym {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::Type(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_type().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

impl Substitutable<Substitution> for ValueSym {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::Value(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_value().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

impl Substitutable<Substitution> for AnySym {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        s.get(self).copied()
    }
}
