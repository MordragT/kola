use std::{collections::HashMap, ops::Index};

use indexmap::IndexMap;
use kola_resolver::symbol::{ModuleSym, Sym, TypeSym, ValueSym};
use kola_tree::node::{ModuleNamespace, TypeNamespace, ValueNamespace};
use kola_utils::{interner::StrKey, scope::LinearScope};

use crate::types::{Kind, ModuleType, MonoType, PolyType, TypeVar};

pub type KindEnv = IndexMap<TypeVar, Vec<Kind>>;

pub trait BoundVars {
    /// Extends the given vector with the type variables that are bound in this type.
    fn extend_bound_vars(&self, vars: &mut Vec<TypeVar>);

    /// Returns the type variables that are bound in this type.
    fn bound_vars(&self) -> Vec<TypeVar> {
        let mut vars = Vec::new();
        self.extend_bound_vars(&mut vars);
        vars.sort_unstable();
        vars.dedup();
        vars
    }
}

pub type LocalTypeEnv = LinearScope<StrKey, MonoType>;

pub type ModuleTypeEnv = HashMap<ValueSym, MonoType>;

#[derive(Debug, Clone, Default)]
pub struct SymbolCache<T>(HashMap<(Sym<T>, Sym<T>), bool>);

impl<T> SymbolCache<T> {
    pub fn new() -> Self {
        SymbolCache(HashMap::new())
    }

    pub fn get(&self, a: Sym<T>, b: Sym<T>) -> Option<bool> {
        let key = if a < b { (a, b) } else { (b, a) };

        self.0.get(&key).copied()
    }

    pub fn insert(&mut self, a: Sym<T>, b: Sym<T>, value: bool) {
        let key = if a < b { (a, b) } else { (b, a) };

        self.0.insert(key, value);
    }
}

impl<T> Index<(Sym<T>, Sym<T>)> for SymbolCache<T> {
    type Output = bool;

    fn index(&self, (a, b): (Sym<T>, Sym<T>)) -> &Self::Output {
        let key = if a < b { (a, b) } else { (b, a) };

        self.0.get(&key).expect("Key not found in SymbolCache")
    }
}

pub type ModuleCache = SymbolCache<ModuleNamespace>;
pub type TypeCache = SymbolCache<TypeNamespace>;
pub type ValueCache = SymbolCache<ValueNamespace>;

// TODO:
// - remove Types inside ModuleType and TypeInfo
// - replace them with *Sym and use the TypeEnvironment to resolve them
// - implement cached equivalence checks for types, modules and values

// ModuleSyms are not unique that's why this doesn't work :(

#[derive(Debug, Clone, Default)]
pub struct GlobalTypeEnv {
    pub values: HashMap<ValueSym, PolyType>,
    pub equiv_values: ValueCache,

    pub types: HashMap<TypeSym, PolyType>,
    pub equiv_types: TypeCache,

    pub modules: HashMap<ModuleSym, ModuleType>,
    pub equiv_modules: ModuleCache,
}

impl GlobalTypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_value(&mut self, sym: ValueSym, ty: PolyType) {
        self.values.insert(sym, ty);
    }

    pub fn insert_type(&mut self, sym: TypeSym, ty: PolyType) {
        self.types.insert(sym, ty);
    }

    pub fn insert_module(&mut self, sym: ModuleSym, ty: ModuleType) {
        self.modules.insert(sym, ty);
    }

    pub fn get_value(&self, sym: ValueSym) -> Option<&PolyType> {
        self.values.get(&sym)
    }

    pub fn get_type(&self, sym: TypeSym) -> Option<&PolyType> {
        self.types.get(&sym)
    }

    pub fn get_module(&self, sym: ModuleSym) -> Option<&ModuleType> {
        self.modules.get(&sym)
    }

    // TODO alpha equivalence isn't properly implemented yet

    // pub fn value_equivalent(&mut self, a: ValueSym, b: ValueSym) -> bool {
    //     if let Some(result) = self.equiv_values.get(a, b) {
    //         return result;
    //     }

    //     let result = match (self.values.get(&a), self.values.get(&b)) {
    //         (Some(a), Some(b)) => a.alpha_equivalent(b),
    //         _ => false,
    //     };

    //     self.equiv_values.insert(a, b, result);
    //     result
    // }

    // pub fn type_equivalent(&mut self, a: TypeSym, b: TypeSym) -> bool {
    //     if let Some(result) = self.equiv_types.get(a, b) {
    //         return result;
    //     }

    //     let result = match (self.types.get(&a), self.types.get(&b)) {
    //         (Some(a), Some(b)) => a.alpha_equivalent(b),
    //         _ => false,
    //     };

    //     self.equiv_types.insert(a, b, result);
    //     result
    // }

    // pub fn module_equivalent(&mut self, a: ModuleSym, b: ModuleSym) -> bool {
    //     if let Some(result) = self.equiv_modules.get(a, b) {
    //         return result;
    //     }

    //     let result = match (self.modules.get(&a), self.modules.get(&b)) {
    //         (Some(a), Some(b)) => a.alpha_equivalent(b, self),
    //         _ => false,
    //     };

    //     self.equiv_modules.insert(a, b, result);
    //     result
    // }
}

impl Index<ValueSym> for GlobalTypeEnv {
    type Output = PolyType;

    fn index(&self, sym: ValueSym) -> &Self::Output {
        self.values
            .get(&sym)
            .expect("ValueSym not found in TypeEnvironment")
    }
}

impl Index<TypeSym> for GlobalTypeEnv {
    type Output = PolyType;

    fn index(&self, sym: TypeSym) -> &Self::Output {
        self.types
            .get(&sym)
            .expect("TypeSym not found in TypeEnvironment")
    }
}

impl Index<ModuleSym> for GlobalTypeEnv {
    type Output = ModuleType;

    fn index(&self, sym: ModuleSym) -> &Self::Output {
        self.modules
            .get(&sym)
            .expect("ModuleSym not found in TypeEnvironment")
    }
}
