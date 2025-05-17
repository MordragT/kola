use std::{collections::HashMap, marker::PhantomData, ops::Index};

use derive_more::{Display, From};
use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{self, Vis},
};
use kola_utils::interner::StrKey;

use super::ModuleKey;
use crate::error::NameCollision;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ModuleScopes(HashMap<ModuleKey, ModuleScope>);

impl ModuleScopes {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn insert(&mut self, module_key: ModuleKey, scope: ModuleScope) {
        self.0.insert(module_key, scope);
    }

    pub fn get(&self, module_key: ModuleKey) -> Option<&ModuleScope> {
        self.0.get(&module_key)
    }
}

impl Index<ModuleKey> for ModuleScopes {
    type Output = ModuleScope;

    fn index(&self, index: ModuleKey) -> &Self::Output {
        self.0.get(&index).unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unresolved;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Resolved;

pub type UnresolvedModuleScope = ModuleScope<Unresolved>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleScope<S = Resolved> {
    pub(crate) module_key: ModuleKey,
    pub(crate) imports: HashMap<Id<node::ModuleImport>, ModuleKey>,
    pub(crate) modules: HashMap<StrKey, ModuleBindInfo>,
    pub(crate) values: HashMap<StrKey, ValueBindInfo>,
    pub(crate) types: HashMap<StrKey, TypeBindInfo>,
    pub(crate) state: PhantomData<S>,
}

impl ModuleScope<Unresolved> {
    pub fn new(module_key: ModuleKey) -> Self {
        Self {
            module_key,
            imports: HashMap::new(),
            modules: HashMap::new(),
            values: HashMap::new(),
            types: HashMap::new(),
            state: PhantomData,
        }
    }

    pub fn insert_import(&mut self, import_id: Id<node::ModuleImport>, module_key: ModuleKey) {
        self.imports.insert(import_id, module_key);
    }

    pub fn insert_module(
        &mut self,
        name: StrKey,
        info: ModuleBindInfo,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self._get(name) {
            return Err(name_collision(info.into(), bind));
        }

        self.modules.insert(name, info);

        Ok(())
    }

    pub fn insert_value(&mut self, name: StrKey, info: ValueBindInfo) -> Result<(), NameCollision> {
        if let Some(bind) = self._get(name) {
            return Err(name_collision(info.into(), bind));
        }

        self.values.insert(name, info);

        Ok(())
    }

    pub fn insert_type(&mut self, name: StrKey, info: TypeBindInfo) -> Result<(), NameCollision> {
        if let Some(bind) = self._get(name) {
            return Err(name_collision(info.into(), bind));
        }

        self.types.insert(name, info);

        Ok(())
    }
}

const fn name_collision(this: BindInfo, other: BindInfo) -> NameCollision {
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
        }
    };

    match this {
        BindInfo::Module(this) => NameCollision::module_bind(this.location, other.location(), help),
        BindInfo::Value(this) => NameCollision::value_bind(this.location, other.location(), help),
        BindInfo::Type(this) => NameCollision::type_bind(this.location, other.location(), help),
    }
}

impl<S> ModuleScope<S> {
    #[inline]
    pub fn module_key(&self) -> ModuleKey {
        self.module_key
    }

    #[inline]
    pub fn exists(&self, name: StrKey) -> bool {
        self._get(name).is_some()
    }

    #[inline]
    fn _get(&self, name: StrKey) -> Option<BindInfo> {
        if let Some(module) = self.modules.get(&name) {
            Some(BindInfo::Module(*module))
        } else if let Some(value) = self.values.get(&name) {
            Some(BindInfo::Value(*value))
        } else if let Some(ty) = self.types.get(&name) {
            Some(BindInfo::Type(*ty))
        } else {
            None
        }
    }
}

impl ModuleScope<Resolved> {
    #[inline]
    pub fn get_module(&self, name: StrKey) -> Option<ModuleBindInfo> {
        self.modules.get(&name).copied()
    }

    #[inline]
    pub fn get_value(&self, name: StrKey) -> Option<ValueBindInfo> {
        self.values.get(&name).copied()
    }

    #[inline]
    pub fn get_type(&self, name: StrKey) -> Option<TypeBindInfo> {
        self.types.get(&name).copied()
    }

    #[inline]
    pub fn get(&self, name: StrKey) -> Option<BindInfo> {
        self._get(name)
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BindKind {
    Module,
    Value,
    Type,
}

impl BindKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            BindKind::Module => "Module",
            BindKind::Value => "Value",
            BindKind::Type => "Type",
        }
    }
}

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BindInfo {
    Module(ModuleBindInfo),
    Value(ValueBindInfo),
    Type(TypeBindInfo),
}

impl BindInfo {
    pub const fn kind(&self) -> BindKind {
        match self {
            BindInfo::Module(_) => BindKind::Module,
            BindInfo::Value(_) => BindKind::Value,
            BindInfo::Type(_) => BindKind::Type,
        }
    }

    pub const fn location(&self) -> Loc {
        match self {
            BindInfo::Module(info) => info.location,
            BindInfo::Value(info) => info.location,
            BindInfo::Type(info) => info.location,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleBindInfo {
    pub bind_id: Id<node::ModuleBind>,
    pub value_id: Id<node::ModuleExpr>,
    pub location: Loc,
    pub vis: Vis,
    pub(crate) module_key: Option<ModuleKey>,
}

impl ModuleBindInfo {
    pub fn new(
        bind_id: Id<node::ModuleBind>,
        value_id: Id<node::ModuleExpr>,
        location: Loc,
        vis: Vis,
    ) -> Self {
        Self {
            bind_id,
            value_id,
            location,
            vis,
            module_key: None,
        }
    }

    pub(crate) fn set_module_key(&mut self, module_key: ModuleKey) {
        self.module_key = Some(module_key);
    }

    pub fn module_key(&self) -> ModuleKey {
        // SAFETY: This is safe because the module key is set when the module is resolved.
        // And the ModuleBindInfo can only be retrieved after the module is resolved.
        self.module_key.unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueBindInfo {
    pub bind_id: Id<node::ValueBind>,
    pub value_id: Id<node::Expr>,
    pub location: Loc,
    pub vis: Vis,
}

impl ValueBindInfo {
    pub fn new(
        bind_id: Id<node::ValueBind>,
        value_id: Id<node::Expr>,
        location: Loc,
        vis: Vis,
    ) -> Self {
        Self {
            bind_id,
            value_id,
            location,
            vis,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeBindInfo {
    pub bind_id: Id<node::TypeBind>,
    pub value_id: Id<node::Type>,
    pub location: Loc,
    // pub vis: Vis,
}

impl TypeBindInfo {
    pub fn new(bind_id: Id<node::TypeBind>, value_id: Id<node::Type>, location: Loc) -> Self {
        Self {
            bind_id,
            value_id,
            location,
        }
    }
}
