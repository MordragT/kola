use std::{hash::Hash, ops::Index};

use kola_collections::{BiMap, HashMap, ShadowMap, hash_map};
use kola_span::Loc;
use kola_tree::{id::Id, node};
use kola_utils::interner::{PathKey, StrKey};

use crate::{
    QualId,
    error::NameCollision,
    info::{AnyInfo, BindInfo, BindKind, ModuleInfo, TypeInfo, ValueInfo},
    symbol::{ModuleSym, Sym, Symbol, TypeSym, ValueSym},
};

pub type LexicalScope = ShadowMap<StrKey, ValueSym>;

#[derive(Debug, Clone)]
pub struct Rib<S, T> {
    pub binds: BiMap<StrKey, S>,
    pub infos: HashMap<S, BindInfo<T>>,
}

impl<S, T> Rib<S, T> {
    #[inline]
    pub fn new() -> Self {
        Self {
            binds: BiMap::new(),
            infos: HashMap::new(),
        }
    }
}
impl<S, T> Rib<S, T>
where
    S: Copy + Eq + Hash,
{
    #[inline]
    pub fn insert(&mut self, name: StrKey, symbol: S, info: BindInfo<T>) {
        self.binds.insert(name, symbol);
        self.infos.insert(symbol, info);
    }

    #[inline]
    pub fn lookup_sym(&self, name: StrKey) -> Option<S> {
        self.binds.get_by_key(&name).copied()
    }

    #[inline]
    pub fn lookup_key(&self, symbol: S) -> Option<StrKey> {
        self.binds.get_by_value(&symbol).copied()
    }

    #[inline]
    pub fn get_by_sym(&self, symbol: S) -> Option<BindInfo<T>> {
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
    pub fn contains_sym(&self, symbol: S) -> bool {
        self.binds.contains_value(&symbol)
    }
}

impl<S, T> IntoIterator for Rib<S, T> {
    type Item = (S, BindInfo<T>);
    type IntoIter = hash_map::IntoIter<S, BindInfo<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.infos.into_iter()
    }
}

impl<'a, S, T> IntoIterator for &'a Rib<S, T> {
    type Item = (&'a S, &'a BindInfo<T>);
    type IntoIter = hash_map::Iter<'a, S, BindInfo<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.infos.iter()
    }
}

impl<'a, S, T> IntoIterator for &'a mut Rib<S, T> {
    type Item = (&'a S, &'a mut BindInfo<T>);
    type IntoIter = hash_map::IterMut<'a, S, BindInfo<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.infos.iter_mut()
    }
}

impl<S, T> Index<StrKey> for Rib<S, T>
where
    S: Copy + Eq + Hash,
{
    type Output = BindInfo<T>;

    fn index(&self, key: StrKey) -> &Self::Output {
        self.lookup_sym(key)
            .and_then(|sym| self.infos.get(&sym))
            .expect("Bind not found")
    }
}

impl<ST, T> Index<Sym<ST>> for Rib<Sym<ST>, T> {
    type Output = BindInfo<T>;

    fn index(&self, sym: Sym<ST>) -> &Self::Output {
        self.infos.get(&sym).expect("Bind not found")
    }
}

#[derive(Debug, Clone)]
pub struct ModuleScope {
    pub id: QualId<node::Module>,
    pub symbol: ModuleSym,
    pub loc: Loc,
    pub modules: Rib<ModuleSym, node::ModuleBind>,
    pub types: Rib<TypeSym, node::TypeBind>,
    pub values: Rib<ValueSym, node::ValueBind>,

    pub paths: BiMap<ModuleSym, Vec<StrKey>>,
}

impl ModuleScope {
    pub fn new(id: QualId<node::Module>, symbol: ModuleSym, loc: Loc) -> Self {
        Self {
            id,
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
        symbol: ModuleSym,
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
        symbol: ValueSym,
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
        symbol: TypeSym,
        info: TypeInfo,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.get(name) {
            return Err(name_collision(info.into(), bind));
        }

        self.types.insert(name, symbol, info);
        Ok(())
    }

    pub fn insert_path(&mut self, symbol: ModuleSym, path: Vec<StrKey>) {
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
    }
}

impl ModuleScope {
    #[inline]
    pub fn symbol(&self) -> ModuleSym {
        self.symbol
    }

    #[inline]
    pub fn id(&self) -> QualId<node::Module> {
        self.id
    }

    #[inline]
    pub fn path_key(&self) -> PathKey {
        self.id.0
    }

    #[inline]
    pub fn node_id(&self) -> Id<node::Module> {
        self.id.1
    }

    #[inline]
    pub fn contains_key(&self, name: StrKey) -> bool {
        self.modules.contains_key(name)
            || self.values.contains_key(name)
            || self.types.contains_key(name)
    }

    #[inline]
    pub fn contains_sym(&self, symbol: Symbol) -> bool {
        match symbol {
            Symbol::Module(symbol) => self.modules.contains_sym(symbol),
            Symbol::Value(symbol) => self.values.contains_sym(symbol),
            Symbol::Type(symbol) => self.types.contains_sym(symbol),
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
