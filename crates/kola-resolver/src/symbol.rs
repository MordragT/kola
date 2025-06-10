use derive_more::From;
use kola_tree::node;
use kola_utils::define_unique_leveled_id;
use std::{
    collections::HashMap,
    hash::Hash,
    marker::PhantomData,
    ops::Index,
    sync::atomic::{AtomicU32, Ordering},
};

use crate::QualId;

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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleTag;
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeTag;
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueTag;

pub type ModuleSym = Sym<ModuleTag>;
pub type TypeSym = Sym<TypeTag>;
pub type ValueSym = Sym<ValueTag>;

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Symbol {
    Module(ModuleSym),
    Type(TypeSym),
    Value(ValueSym),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SymbolKind {
    Module,
    Type,
    Value,
}

impl Symbol {
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

    pub fn kind(&self) -> SymbolKind {
        match self {
            Self::Module(_) => SymbolKind::Module,
            Self::Type(_) => SymbolKind::Type,
            Self::Value(_) => SymbolKind::Value,
        }
    }
}

pub type Lookup<T, S> = HashMap<QualId<T>, S>;

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub module_binds: Lookup<node::ModuleBind, ModuleSym>,
    pub module_imports: Lookup<node::ModuleImport, ModuleSym>,
    pub modules: Lookup<node::Module, ModuleSym>,
    pub module_paths: Lookup<node::ModulePath, ModuleSym>,
    pub type_binds: Lookup<node::TypeBind, TypeSym>,
    pub value_binds: Lookup<node::ValueBind, ValueSym>,
    pub let_exprs: Lookup<node::LetExpr, ValueSym>,
    pub lambda_exprs: Lookup<node::LambdaExpr, ValueSym>,
    pub path_exprs: Lookup<node::PathExpr, ValueSym>,
}

impl SymbolTable {
    #[inline]
    pub fn new() -> Self {
        Self {
            module_binds: Lookup::new(),
            module_imports: Lookup::new(),
            modules: Lookup::new(),
            module_paths: Lookup::new(),
            type_binds: Lookup::new(),
            value_binds: Lookup::new(),
            let_exprs: Lookup::new(),
            lambda_exprs: Lookup::new(),
            path_exprs: Lookup::new(),
        }
    }

    #[inline]
    pub fn insert_module_bind(&mut self, id: QualId<node::ModuleBind>, sym: ModuleSym) {
        self.module_binds.insert(id, sym);
    }

    #[inline]
    pub fn insert_module_import(&mut self, id: QualId<node::ModuleImport>, sym: ModuleSym) {
        self.module_imports.insert(id, sym);
    }

    #[inline]
    pub fn insert_module(&mut self, id: QualId<node::Module>, sym: ModuleSym) {
        self.modules.insert(id, sym);
    }

    #[inline]
    pub fn insert_module_path(&mut self, id: QualId<node::ModulePath>, sym: ModuleSym) {
        self.module_paths.insert(id, sym);
    }

    #[inline]
    pub fn insert_type_bind(&mut self, id: QualId<node::TypeBind>, sym: TypeSym) {
        self.type_binds.insert(id, sym);
    }

    #[inline]
    pub fn insert_value_bind(&mut self, id: QualId<node::ValueBind>, sym: ValueSym) {
        self.value_binds.insert(id, sym);
    }

    #[inline]
    pub fn insert_let_expr(&mut self, id: QualId<node::LetExpr>, sym: ValueSym) {
        self.let_exprs.insert(id, sym);
    }

    #[inline]
    pub fn insert_lambda_expr(&mut self, id: QualId<node::LambdaExpr>, sym: ValueSym) {
        self.lambda_exprs.insert(id, sym);
    }

    #[inline]
    pub fn insert_path_expr(&mut self, id: QualId<node::PathExpr>, sym: ValueSym) {
        self.path_exprs.insert(id, sym);
    }

    #[inline]
    pub fn lookup_module_bind(&self, id: QualId<node::ModuleBind>) -> Option<ModuleSym> {
        self.module_binds.get(&id).copied()
    }

    #[inline]
    pub fn lookup_module_import(&self, id: QualId<node::ModuleImport>) -> Option<ModuleSym> {
        self.module_imports.get(&id).copied()
    }

    #[inline]
    pub fn lookup_module(&self, id: QualId<node::Module>) -> Option<ModuleSym> {
        self.modules.get(&id).copied()
    }

    #[inline]
    pub fn lookup_module_path(&self, id: QualId<node::ModulePath>) -> Option<ModuleSym> {
        self.module_paths.get(&id).copied()
    }

    #[inline]
    pub fn lookup_type_bind(&self, id: QualId<node::TypeBind>) -> Option<TypeSym> {
        self.type_binds.get(&id).copied()
    }

    #[inline]
    pub fn lookup_value_bind(&self, id: QualId<node::ValueBind>) -> Option<ValueSym> {
        self.value_binds.get(&id).copied()
    }

    #[inline]
    pub fn lookup_let_expr(&self, id: QualId<node::LetExpr>) -> Option<ValueSym> {
        self.let_exprs.get(&id).copied()
    }

    #[inline]
    pub fn lookup_lambda_expr(&self, id: QualId<node::LambdaExpr>) -> Option<ValueSym> {
        self.lambda_exprs.get(&id).copied()
    }

    #[inline]
    pub fn lookup_path_expr(&self, id: QualId<node::PathExpr>) -> Option<ValueSym> {
        self.path_exprs.get(&id).copied()
    }
}

impl Index<QualId<node::ModuleBind>> for SymbolTable {
    type Output = ModuleSym;

    fn index(&self, index: QualId<node::ModuleBind>) -> &Self::Output {
        self.module_binds
            .get(&index)
            .expect("Module symbol not found")
    }
}

impl Index<QualId<node::ModuleImport>> for SymbolTable {
    type Output = ModuleSym;

    fn index(&self, index: QualId<node::ModuleImport>) -> &Self::Output {
        self.module_imports
            .get(&index)
            .expect("Module import symbol not found")
    }
}

impl Index<QualId<node::Module>> for SymbolTable {
    type Output = ModuleSym;

    fn index(&self, index: QualId<node::Module>) -> &Self::Output {
        self.modules.get(&index).expect("Module symbol not found")
    }
}

impl Index<QualId<node::ModulePath>> for SymbolTable {
    type Output = ModuleSym;

    fn index(&self, index: QualId<node::ModulePath>) -> &Self::Output {
        self.module_paths
            .get(&index)
            .expect("Module path symbol not found")
    }
}

impl Index<QualId<node::TypeBind>> for SymbolTable {
    type Output = TypeSym;

    fn index(&self, index: QualId<node::TypeBind>) -> &Self::Output {
        self.type_binds.get(&index).expect("Type symbol not found")
    }
}

impl Index<QualId<node::ValueBind>> for SymbolTable {
    type Output = ValueSym;

    fn index(&self, index: QualId<node::ValueBind>) -> &Self::Output {
        self.value_binds
            .get(&index)
            .expect("Value symbol not found")
    }
}

impl Index<QualId<node::LetExpr>> for SymbolTable {
    type Output = ValueSym;

    fn index(&self, index: QualId<node::LetExpr>) -> &Self::Output {
        self.let_exprs
            .get(&index)
            .expect("Let expression symbol not found")
    }
}

impl Index<QualId<node::LambdaExpr>> for SymbolTable {
    type Output = ValueSym;

    fn index(&self, index: QualId<node::LambdaExpr>) -> &Self::Output {
        self.lambda_exprs
            .get(&index)
            .expect("Lambda expression symbol not found")
    }
}

impl Index<QualId<node::PathExpr>> for SymbolTable {
    type Output = ValueSym;

    fn index(&self, index: QualId<node::PathExpr>) -> &Self::Output {
        self.path_exprs
            .get(&index)
            .expect("Path expression symbol not found")
    }
}
