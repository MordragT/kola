use derive_more::From;
use kola_collections::{HashMap, hash_map};
use kola_tree::node::{
    ModuleNamespace, Name, Namespace, NamespaceKind, TypeNamespace, ValueNamespace,
};
use kola_utils::define_unique_leveled_id;
use std::{
    hash::Hash,
    marker::PhantomData,
    ops::Index,
    sync::atomic::{AtomicU32, Ordering},
};

static LEVEL: AtomicU32 = AtomicU32::new(0);
static GENERATOR: AtomicU32 = AtomicU32::new(0);

define_unique_leveled_id!(Sym);

impl<T: ?Sized> Sym<T> {
    pub fn new() -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
        let level = Self::load_level();
        Self {
            id,
            level,
            t: PhantomData,
        }
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

pub type ModuleSym = Sym<ModuleNamespace>;
pub type TypeSym = Sym<TypeNamespace>;
pub type ValueSym = Sym<ValueNamespace>;

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnySym {
    Module(ModuleSym),
    Type(TypeSym),
    Value(ValueSym),
}

impl AnySym {
    pub fn id(&self) -> u32 {
        match self {
            Self::Module(symbol) => symbol.id(),
            Self::Type(symbol) => symbol.id(),
            Self::Value(symbol) => symbol.id(),
        }
    }

    pub fn as_usize(&self) -> usize {
        self.id() as usize
    }

    pub fn level(&self) -> u32 {
        match self {
            Self::Module(symbol) => symbol.level(),
            Self::Type(symbol) => symbol.level(),
            Self::Value(symbol) => symbol.level(),
        }
    }

    pub fn kind(&self) -> NamespaceKind {
        match self {
            Self::Module(_) => NamespaceKind::Module,
            Self::Type(_) => NamespaceKind::Type,
            Self::Value(_) => NamespaceKind::Value,
        }
    }
}
