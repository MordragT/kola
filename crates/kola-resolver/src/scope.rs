use std::ops::Index;

use kola_collections::{BiMap, HashMap, hash_map};
use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{
        self, AnyName, ModuleName, ModuleNamespace, Name, Namespace, TypeName, TypeNamespace,
        ValueName, ValueNamespace,
    },
};
use kola_utils::interner::{PathKey, StrKey};

use crate::{
    QualId,
    error::NameCollision,
    info::{AnyInfo, BindInfo, ModuleInfo, TypeInfo, ValueInfo},
    symbol::{AnySym, ModuleSym, Sym, TypeSym, ValueSym},
};

#[derive(Debug, Clone)]
pub struct Rib<N: Namespace> {
    pub binds: BiMap<Name<N>, Sym<N>>,
    pub infos: HashMap<Sym<N>, BindInfo<N>>,
}

impl<N: Namespace> Rib<N> {
    #[inline]
    pub fn new() -> Self {
        Self {
            binds: BiMap::new(),
            infos: HashMap::new(),
        }
    }
}
impl<N: Namespace> Rib<N> {
    #[inline]
    pub fn insert(&mut self, name: Name<N>, symbol: Sym<N>, info: BindInfo<N>) {
        self.binds.insert(name, symbol);
        self.infos.insert(symbol, info);
    }

    #[inline]
    pub fn lookup_sym(&self, name: Name<N>) -> Option<Sym<N>> {
        self.binds.get_by_key(&name).copied()
    }

    #[inline]
    pub fn lookup_key(&self, symbol: Sym<N>) -> Option<Name<N>> {
        self.binds.get_by_value(&symbol).copied()
    }

    #[inline]
    pub fn get_by_sym(&self, symbol: Sym<N>) -> Option<BindInfo<N>> {
        self.infos.get(&symbol).copied()
    }

    #[inline]
    pub fn get_by_key(&self, name: Name<N>) -> Option<BindInfo<N>> {
        self.lookup_sym(name)
            .and_then(|symbol| self.get_by_sym(symbol))
    }

    #[inline]
    pub fn contains_key(&self, name: Name<N>) -> bool {
        self.binds.contains_key(&name)
    }

    #[inline]
    pub fn contains_sym(&self, symbol: Sym<N>) -> bool {
        self.binds.contains_value(&symbol)
    }
}

impl<N: Namespace> IntoIterator for Rib<N> {
    type Item = (Sym<N>, BindInfo<N>);
    type IntoIter = hash_map::IntoIter<Sym<N>, BindInfo<N>>;

    fn into_iter(self) -> Self::IntoIter {
        self.infos.into_iter()
    }
}

impl<'a, N: Namespace> IntoIterator for &'a Rib<N> {
    type Item = (&'a Sym<N>, &'a BindInfo<N>);
    type IntoIter = hash_map::Iter<'a, Sym<N>, BindInfo<N>>;

    fn into_iter(self) -> Self::IntoIter {
        self.infos.iter()
    }
}

impl<'a, N: Namespace> IntoIterator for &'a mut Rib<N> {
    type Item = (&'a Sym<N>, &'a mut BindInfo<N>);
    type IntoIter = hash_map::IterMut<'a, Sym<N>, BindInfo<N>>;

    fn into_iter(self) -> Self::IntoIter {
        self.infos.iter_mut()
    }
}

impl<N: Namespace> Index<Name<N>> for Rib<N> {
    type Output = BindInfo<N>;

    fn index(&self, key: Name<N>) -> &Self::Output {
        self.lookup_sym(key)
            .and_then(|sym| self.infos.get(&sym))
            .expect("Bind not found")
    }
}

impl<N: Namespace> Index<Sym<N>> for Rib<N> {
    type Output = BindInfo<N>;

    fn index(&self, sym: Sym<N>) -> &Self::Output {
        self.infos.get(&sym).expect("Bind not found")
    }
}

#[derive(Debug, Clone)]
pub struct ModuleScope {
    pub id: QualId<node::Module>,
    pub symbol: ModuleSym,
    pub loc: Loc,
    pub modules: Rib<ModuleNamespace>,
    pub types: Rib<TypeNamespace>,
    pub values: Rib<ValueNamespace>,

    pub paths: BiMap<ModuleSym, Vec<ModuleName>>,
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
        name: ModuleName,
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
        name: ValueName,
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
        name: TypeName,
        symbol: TypeSym,
        info: TypeInfo,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.get(name) {
            return Err(name_collision(info.into(), bind));
        }

        self.types.insert(name, symbol, info);
        Ok(())
    }

    pub fn insert_path(&mut self, symbol: ModuleSym, path: Vec<ModuleName>) {
        self.paths.insert(symbol, path);
    }
}

const fn name_collision(this: AnyInfo, other: AnyInfo) -> NameCollision {
    use kola_tree::node::NamespaceKind::*;

    let help = match (this.kind(), other.kind()) {
        (Module, Module) => "Module bindings must have distinct names from Module bindings.",
        (Module, Value) => "Module bindings must have distinct names from Value bindings.",
        (Module, Type) => "Module bindings must have distinct names from Type bindings.",
        (Value, Module) => "Value bindings must have distinct names from Module bindings.",
        (Value, Value) => "Value bindings must have distinct names from Value bindings.",
        (Value, Type) => "Value bindings must have distinct names from Type bindings.",
        (Type, Module) => "Type bindings must have distinct names from Module bindings.",
        (Type, Value) => "Type bindings must have distinct names from Value bindings.",
        (Type, Type) => "Type bindings must have distinct names from Type bindings.",
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
    pub fn contains_name(&self, name: AnyName) -> bool {
        match name {
            AnyName::Module(name) => self.modules.contains_key(name),
            AnyName::Value(name) => self.values.contains_key(name),
            AnyName::Type(name) => self.types.contains_key(name),
        }
    }

    #[inline]
    pub fn contains_sym(&self, symbol: AnySym) -> bool {
        match symbol {
            AnySym::Module(symbol) => self.modules.contains_sym(symbol),
            AnySym::Value(symbol) => self.values.contains_sym(symbol),
            AnySym::Type(symbol) => self.types.contains_sym(symbol),
        }
    }

    #[inline]
    pub fn get_module(&self, name: ModuleName) -> Option<ModuleInfo> {
        self.modules.get_by_key(name)
    }

    #[inline]
    pub fn get_value(&self, name: ValueName) -> Option<ValueInfo> {
        self.values.get_by_key(name)
    }

    #[inline]
    pub fn get_type(&self, name: TypeName) -> Option<TypeInfo> {
        self.types.get_by_key(name)
    }

    #[inline]
    fn get(&self, name: impl Into<AnyName>) -> Option<AnyInfo> {
        match name.into() {
            AnyName::Module(name) => self.modules.get_by_key(name).map(AnyInfo::Module),
            AnyName::Value(name) => self.values.get_by_key(name).map(AnyInfo::Value),
            AnyName::Type(name) => self.types.get_by_key(name).map(AnyInfo::Type),
        }
    }
}
