use std::{
    collections::{HashMap, hash_map},
    ops::Index,
};

use kola_collections::shadow_map::ShadowMap;
use kola_span::Loc;
use kola_tree::node;
use kola_utils::{bimap::BiMap, interner::StrKey};

use crate::{
    error::NameCollision,
    info::{AnyInfo, BindInfo, BindKind, ModuleInfo, TypeInfo, ValueInfo},
    symbol::{AnySymbol, LocalSymbol, ModuleSymbol, Symbol, TypeSymbol, ValueSymbol},
};

pub type LexicalScope = ShadowMap<StrKey, LocalSymbol>;

#[derive(Debug, Clone)]
pub struct Rib<T> {
    pub binds: BiMap<StrKey, Symbol<T>>,
    pub infos: HashMap<Symbol<T>, BindInfo<T>>,
}

impl<T> Rib<T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            binds: BiMap::new(),
            infos: HashMap::new(),
        }
    }

    #[inline]
    pub fn insert(&mut self, name: StrKey, symbol: Symbol<T>, info: BindInfo<T>) {
        self.binds.insert(name, symbol);
        self.infos.insert(symbol, info);
    }

    #[inline]
    pub fn lookup_sym(&self, name: StrKey) -> Option<Symbol<T>> {
        self.binds.get_by_key(&name).copied()
    }

    #[inline]
    pub fn lookup_key(&self, symbol: Symbol<T>) -> Option<StrKey> {
        self.binds.get_by_value(&symbol).copied()
    }

    #[inline]
    pub fn get_by_sym(&self, symbol: Symbol<T>) -> Option<BindInfo<T>> {
        self.infos.get(&symbol).copied()
    }

    #[inline]
    pub fn get_by_key(&self, name: StrKey) -> Option<BindInfo<T>> {
        self.lookup_sym(name)
            .and_then(|symbol| self.get_by_sym(symbol))
    }

    #[inline]
    pub fn contains_key(&self, name: StrKey) -> bool {
        self.binds.contains_key(&name)
    }

    #[inline]
    pub fn contains_sym(&self, symbol: Symbol<T>) -> bool {
        self.binds.contains_value(&symbol)
    }
}

impl<T> IntoIterator for Rib<T> {
    type Item = (Symbol<T>, BindInfo<T>);
    type IntoIter = hash_map::IntoIter<Symbol<T>, BindInfo<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.infos.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Rib<T> {
    type Item = (&'a Symbol<T>, &'a BindInfo<T>);
    type IntoIter = hash_map::Iter<'a, Symbol<T>, BindInfo<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.infos.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut Rib<T> {
    type Item = (&'a Symbol<T>, &'a mut BindInfo<T>);
    type IntoIter = hash_map::IterMut<'a, Symbol<T>, BindInfo<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.infos.iter_mut()
    }
}

impl<T> Index<StrKey> for Rib<T> {
    type Output = BindInfo<T>;

    fn index(&self, key: StrKey) -> &Self::Output {
        self.lookup_sym(key)
            .and_then(|sym| self.infos.get(&sym))
            .expect("Bind not found")
    }
}

impl<T> Index<Symbol<T>> for Rib<T> {
    type Output = BindInfo<T>;

    fn index(&self, sym: Symbol<T>) -> &Self::Output {
        self.infos.get(&sym).expect("Bind not found")
    }
}

pub struct ModuleScope {
    pub symbol: ModuleSymbol,
    pub loc: Loc,
    pub modules: Rib<node::ModuleBind>,
    pub types: Rib<node::TypeBind>,
    pub values: Rib<node::ValueBind>,

    pub paths: BiMap<ModuleSymbol, Vec<StrKey>>,
}

impl ModuleScope {
    pub fn new(symbol: ModuleSymbol, loc: Loc) -> Self {
        Self {
            symbol,
            loc,
            modules: Rib::new(),
            types: Rib::new(),
            values: Rib::new(),

            paths: BiMap::new(),
        }
    }

    pub fn insert_module(
        &mut self,
        name: StrKey,
        symbol: ModuleSymbol,
        info: ModuleInfo,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.get(name) {
            return Err(name_collision(info.into(), bind));
        }

        self.modules.insert(name, symbol, info);
        Ok(())
    }

    pub fn insert_value(
        &mut self,
        name: StrKey,
        symbol: ValueSymbol,
        info: ValueInfo,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.get(name) {
            return Err(name_collision(info.into(), bind));
        }

        self.values.insert(name, symbol, info);
        Ok(())
    }

    pub fn insert_type(
        &mut self,
        name: StrKey,
        symbol: TypeSymbol,
        info: TypeInfo,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.get(name) {
            return Err(name_collision(info.into(), bind));
        }

        self.types.insert(name, symbol, info);
        Ok(())
    }

    pub fn insert_path(&mut self, symbol: ModuleSymbol, path: Vec<StrKey>) {
        self.paths.insert(symbol, path);
    }
}

const fn name_collision(this: AnyInfo, other: AnyInfo) -> NameCollision {
    let help = match (this.kind(), other.kind()) {
        (BindKind::Module, BindKind::Module) => {
            "Module bindings must have distinct names from Module bindings."
        }
        (BindKind::Module, BindKind::Value) => {
            "Module bindings must have distinct names from Value bindings."
        }
        (BindKind::Module, BindKind::Type) => {
            "Module bindings must have distinct names from Type bindings."
        }
        (BindKind::Value, BindKind::Module) => {
            "Value bindings must have distinct names from Module bindings."
        }
        (BindKind::Value, BindKind::Value) => {
            "Value bindings must have distinct names from Value bindings."
        }
        (BindKind::Value, BindKind::Type) => {
            "Value bindings must have distinct names from Type bindings."
        }
        (BindKind::Type, BindKind::Module) => {
            "Type bindings must have distinct names from Module bindings."
        }
        (BindKind::Type, BindKind::Value) => {
            "Type bindings must have distinct names from Value bindings."
        }
        (BindKind::Type, BindKind::Type) => {
            "Type bindings must have distinct names from Type bindings."
        } // _ => todo!(),
    };

    match this {
        AnyInfo::Module(this) => NameCollision::module_bind(this.loc, other.location(), help),
        AnyInfo::Value(this) => NameCollision::value_bind(this.loc, other.location(), help),
        AnyInfo::Type(this) => NameCollision::type_bind(this.loc, other.location(), help),
        // AnyInfo::Local(this) => NameCollision::local_bind(this.loc, other.location(), help),
    }
}

impl ModuleScope {
    #[inline]
    pub fn symbol(&self) -> ModuleSymbol {
        self.symbol
    }

    #[inline]
    pub fn contains_key(&self, name: StrKey) -> bool {
        self.modules.contains_key(name)
            || self.values.contains_key(name)
            || self.types.contains_key(name)
    }

    #[inline]
    pub fn contains_sym(&self, symbol: AnySymbol) -> bool {
        match symbol {
            AnySymbol::Module(symbol) => self.modules.contains_sym(symbol),
            AnySymbol::Value(symbol) => self.values.contains_sym(symbol),
            AnySymbol::Type(symbol) => self.types.contains_sym(symbol),
        }
    }

    #[inline]
    pub fn get_module(&self, name: StrKey) -> Option<ModuleInfo> {
        self.modules.get_by_key(name)
    }

    #[inline]
    pub fn get_value(&self, name: StrKey) -> Option<ValueInfo> {
        self.values.get_by_key(name)
    }

    #[inline]
    pub fn get_type(&self, name: StrKey) -> Option<TypeInfo> {
        self.types.get_by_key(name)
    }

    #[inline]
    fn get(&self, name: StrKey) -> Option<AnyInfo> {
        if let Some(module) = self.modules.get_by_key(name) {
            Some(module.into())
        } else if let Some(value) = self.values.get_by_key(name) {
            Some(value.into())
        } else if let Some(ty) = self.types.get_by_key(name) {
            Some(ty.into())
        } else {
            None
        }
    }
}
