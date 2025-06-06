use std::{
    collections::HashMap,
    fmt,
    hash::Hash,
    marker::PhantomData,
    ops::Index,
    sync::atomic::{AtomicU32, Ordering},
};

use derive_more::From;
use kola_tree::{id::Id, node};
use kola_utils::{bimap::BiMap, interner::PathKey};

use crate::info::{BindKind, ModuleDef, TypeDef, ValueDef};

static LEVEL: AtomicU32 = AtomicU32::new(0);
static GENERATOR: AtomicU32 = AtomicU32::new(0);

pub struct Symbol<T> {
    id: u32,
    level: u32,
    t: PhantomData<T>,
}

impl<T> Symbol<T> {
    pub fn new() -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
        let level = Self::load_level();
        Self {
            id,
            level,
            t: PhantomData,
        }
    }

    pub fn level(&self) -> u32 {
        self.level
    }

    pub fn id(&self) -> u32 {
        self.id
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

    pub fn branch<F, U>(mut f: F) -> U
    where
        F: FnMut() -> U,
    {
        LEVEL.fetch_add(1, Ordering::Relaxed);
        let result = f();
        LEVEL.fetch_sub(1, Ordering::Relaxed);
        result
    }
}

impl<T> Default for Symbol<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> fmt::Debug for Symbol<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol({}, {}, {:?})", self.id, self.level, self.t)
    }
}

impl<T> Clone for Symbol<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            level: self.level,
            t: PhantomData,
        }
    }
}

impl<T> Copy for Symbol<T> {}

impl<T> PartialEq for Symbol<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.level == other.level
    }
}

impl<T> Eq for Symbol<T> {}

impl<T> PartialOrd for Symbol<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Symbol<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.id, self.level).cmp(&(other.id, other.level))
    }
}

impl<T> Hash for Symbol<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.level.hash(state);
    }
}

pub type ModuleSymbol = Symbol<node::ModuleBind>;
pub type TypeSymbol = Symbol<node::TypeBind>;
pub type ValueSymbol = Symbol<node::ValueBind>;
pub type LocalSymbol = Symbol<node::LetExpr>;

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnySymbol {
    Module(ModuleSymbol),
    Type(TypeSymbol),
    Value(ValueSymbol),
    // Local(LocalSymbol),
}

impl AnySymbol {
    pub fn id(&self) -> u32 {
        match self {
            AnySymbol::Module(symbol) => symbol.id(),
            AnySymbol::Type(symbol) => symbol.id(),
            AnySymbol::Value(symbol) => symbol.id(),
            // AnySymbol::Local(symbol) => symbol.id(),
        }
    }

    pub fn level(&self) -> u32 {
        match self {
            AnySymbol::Module(symbol) => symbol.level(),
            AnySymbol::Type(symbol) => symbol.level(),
            AnySymbol::Value(symbol) => symbol.level(),
            // AnySymbol::Local(symbol) => symbol.level(),
        }
    }

    pub fn kind(&self) -> BindKind {
        match self {
            AnySymbol::Module(_) => BindKind::Module,
            AnySymbol::Type(_) => BindKind::Type,
            AnySymbol::Value(_) => BindKind::Value,
            // AnySymbol::Local(_) => BindKind::Local,
        }
    }
}

pub type Symbols<T> = BiMap<(PathKey, Id<T>), Symbol<T>>;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub modules: Symbols<node::ModuleBind>,
    pub types: Symbols<node::TypeBind>,
    pub values: Symbols<node::ValueBind>,
    pub locals: Symbols<node::LetExpr>,

    pub imports: BiMap<(PathKey, Id<node::ModuleImport>), ModuleSymbol>,
    pub inlines: BiMap<(PathKey, Id<node::Module>), ModuleSymbol>,
    pub paths: BiMap<(PathKey, Id<node::ModulePath>), ModuleSymbol>,
}

impl SymbolTable {
    #[inline]
    pub fn new() -> Self {
        Self {
            modules: Symbols::new(),
            types: Symbols::new(),
            values: Symbols::new(),
            locals: Symbols::new(),

            imports: BiMap::new(),
            inlines: BiMap::new(),
            paths: BiMap::new(),
        }
    }

    #[inline]
    pub fn insert_module_bind(
        &mut self,
        key: PathKey,
        id: Id<node::ModuleBind>,
        sym: ModuleSymbol,
    ) {
        self.modules.insert((key, id), sym)
    }

    #[inline]
    pub fn insert_type_bind(&mut self, key: PathKey, id: Id<node::TypeBind>, sym: TypeSymbol) {
        self.types.insert((key, id), sym)
    }

    #[inline]
    pub fn insert_value_bind(&mut self, key: PathKey, id: Id<node::ValueBind>, sym: ValueSymbol) {
        self.values.insert((key, id), sym)
    }

    #[inline]
    pub fn insert_local_bind(&mut self, key: PathKey, id: Id<node::LetExpr>, sym: LocalSymbol) {
        self.locals.insert((key, id), sym);
    }

    #[inline]
    pub fn insert_import(&mut self, key: PathKey, id: Id<node::ModuleImport>, sym: ModuleSymbol) {
        self.imports.insert((key, id), sym);
    }

    #[inline]
    pub fn insert_inline(&mut self, key: PathKey, id: Id<node::Module>, sym: ModuleSymbol) {
        self.inlines.insert((key, id), sym);
    }

    #[inline]
    pub fn insert_path(&mut self, key: PathKey, id: Id<node::ModulePath>, sym: ModuleSymbol) {
        self.paths.insert((key, id), sym);
    }

    #[inline]
    pub fn lookup_module_sym(
        &self,
        key: PathKey,
        id: Id<node::ModuleBind>,
    ) -> Option<ModuleSymbol> {
        self.modules.get_by_key(&(key, id)).copied()
    }

    #[inline]
    pub fn lookup_type_sym(&self, key: PathKey, id: Id<node::TypeBind>) -> Option<TypeSymbol> {
        self.types.get_by_key(&(key, id)).copied()
    }

    #[inline]
    pub fn lookup_value_sym(&self, key: PathKey, id: Id<node::ValueBind>) -> Option<ValueSymbol> {
        self.values.get_by_key(&(key, id)).copied()
    }

    #[inline]
    pub fn lookup_module_id(&self, sym: ModuleSymbol) -> Option<(PathKey, Id<node::ModuleBind>)> {
        self.modules.get_by_value(&sym).copied()
    }
    #[inline]
    pub fn lookup_type_id(&self, sym: TypeSymbol) -> Option<(PathKey, Id<node::TypeBind>)> {
        self.types.get_by_value(&sym).copied()
    }

    #[inline]
    pub fn lookup_value_id(&self, sym: ValueSymbol) -> Option<(PathKey, Id<node::ValueBind>)> {
        self.values.get_by_value(&sym).copied()
    }
}

impl Index<ModuleSymbol> for SymbolTable {
    type Output = (PathKey, Id<node::ModuleBind>);

    fn index(&self, index: ModuleSymbol) -> &Self::Output {
        self.modules
            .get_by_value(&index)
            .expect("Module symbol not found")
    }
}

impl Index<TypeSymbol> for SymbolTable {
    type Output = (PathKey, Id<node::TypeBind>);

    fn index(&self, index: TypeSymbol) -> &Self::Output {
        self.types
            .get_by_value(&index)
            .expect("Type symbol not found")
    }
}

impl Index<ValueSymbol> for SymbolTable {
    type Output = (PathKey, Id<node::ValueBind>);

    fn index(&self, index: ValueSymbol) -> &Self::Output {
        self.values
            .get_by_value(&index)
            .expect("Value symbol not found")
    }
}

impl Index<(PathKey, Id<node::ModuleBind>)> for SymbolTable {
    type Output = ModuleSymbol;

    fn index(&self, index: (PathKey, Id<node::ModuleBind>)) -> &Self::Output {
        self.modules
            .get_by_key(&index)
            .expect("Module symbol not found")
    }
}

impl Index<(PathKey, Id<node::TypeBind>)> for SymbolTable {
    type Output = TypeSymbol;

    fn index(&self, index: (PathKey, Id<node::TypeBind>)) -> &Self::Output {
        self.types
            .get_by_key(&index)
            .expect("Type symbol not found")
    }
}

impl Index<(PathKey, Id<node::ValueBind>)> for SymbolTable {
    type Output = ValueSymbol;

    fn index(&self, index: (PathKey, Id<node::ValueBind>)) -> &Self::Output {
        self.values
            .get_by_key(&index)
            .expect("Value symbol not found")
    }
}

pub struct DefTable {
    pub modules: HashMap<ModuleSymbol, ModuleDef>,
    pub types: HashMap<TypeSymbol, TypeDef>,
    pub values: HashMap<ValueSymbol, ValueDef>,
}

impl DefTable {
    #[inline]
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            types: HashMap::new(),
            values: HashMap::new(),
        }
    }

    #[inline]
    pub fn insert_module(&mut self, sym: ModuleSymbol, def: ModuleDef) {
        self.modules.insert(sym, def);
    }

    #[inline]
    pub fn insert_type(&mut self, sym: TypeSymbol, def: TypeDef) {
        self.types.insert(sym, def);
    }

    #[inline]
    pub fn insert_value(&mut self, sym: ValueSymbol, def: ValueDef) {
        self.values.insert(sym, def);
    }
}
