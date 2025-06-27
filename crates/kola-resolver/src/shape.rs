use std::ops::Index;

use crate::symbol::{AnySym, ModuleSym, ModuleTypeSym, TypeSym, ValueSym};
use kola_collections::HashMap;
use kola_tree::node::{AnyName, ModuleName, ModuleTypeName, TypeName, ValueName};

#[derive(Debug, Clone, Default)]
pub struct Shape {
    module_types: HashMap<ModuleTypeName, ModuleTypeSym>,
    modules: HashMap<ModuleName, ModuleSym>,
    types: HashMap<TypeName, TypeSym>,
    values: HashMap<ValueName, ValueSym>,
}

impl Shape {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_module_type(
        &mut self,
        name: ModuleTypeName,
        sym: ModuleTypeSym,
    ) -> Option<ModuleTypeSym> {
        self.module_types.insert(name, sym)
    }

    pub fn insert_module(&mut self, name: ModuleName, sym: ModuleSym) -> Option<ModuleSym> {
        self.modules.insert(name, sym)
    }

    pub fn insert_type(&mut self, name: TypeName, sym: TypeSym) -> Option<TypeSym> {
        self.types.insert(name, sym)
    }

    pub fn insert_value(&mut self, name: ValueName, sym: ValueSym) -> Option<ValueSym> {
        self.values.insert(name, sym)
    }

    #[inline]
    pub fn contains_name(&self, name: AnyName) -> bool {
        match name {
            AnyName::ModuleType(name) => self.module_types.contains_key(&name),
            AnyName::Module(name) => self.modules.contains_key(&name),
            AnyName::Value(name) => self.values.contains_key(&name),
            AnyName::Type(name) => self.types.contains_key(&name),
        }
    }

    #[inline]
    pub fn get_module_type(&self, name: ModuleTypeName) -> Option<ModuleTypeSym> {
        self.module_types.get(&name).copied()
    }

    #[inline]
    pub fn get_module(&self, name: ModuleName) -> Option<ModuleSym> {
        self.modules.get(&name).copied()
    }

    #[inline]
    pub fn get_type(&self, name: TypeName) -> Option<TypeSym> {
        self.types.get(&name).copied()
    }

    #[inline]
    pub fn get_value(&self, name: ValueName) -> Option<ValueSym> {
        self.values.get(&name).copied()
    }

    #[inline]
    pub fn get(&self, name: impl Into<AnyName>) -> Option<AnySym> {
        match name.into() {
            AnyName::ModuleType(name) => self.get_module_type(name).map(AnySym::ModuleType),
            AnyName::Module(name) => self.get_module(name).map(AnySym::Module),
            AnyName::Value(name) => self.get_value(name).map(AnySym::Value),
            AnyName::Type(name) => self.get_type(name).map(AnySym::Type),
        }
    }

    #[inline]
    pub fn iter_module_types(&self) -> impl Iterator<Item = (ModuleTypeName, ModuleTypeSym)> {
        self.module_types.iter().map(|(&name, &sym)| (name, sym))
    }

    #[inline]
    pub fn iter_modules(&self) -> impl Iterator<Item = (ModuleName, ModuleSym)> {
        self.modules.iter().map(|(&name, &sym)| (name, sym))
    }

    #[inline]
    pub fn iter_types(&self) -> impl Iterator<Item = (TypeName, TypeSym)> {
        self.types.iter().map(|(&name, &sym)| (name, sym))
    }

    #[inline]
    pub fn iter_values(&self) -> impl Iterator<Item = (ValueName, ValueSym)> {
        self.values.iter().map(|(&name, &sym)| (name, sym))
    }

    pub fn into_raw(
        self,
    ) -> (
        HashMap<ModuleName, ModuleSym>,
        HashMap<TypeName, TypeSym>,
        HashMap<ValueName, ValueSym>,
    ) {
        (self.modules, self.types, self.values)
    }
}

impl Index<ModuleTypeName> for Shape {
    type Output = ModuleTypeSym;

    fn index(&self, index: ModuleTypeName) -> &Self::Output {
        &self.module_types[&index]
    }
}

impl Index<ModuleName> for Shape {
    type Output = ModuleSym;

    fn index(&self, index: ModuleName) -> &Self::Output {
        &self.modules[&index]
    }
}

impl Index<ValueName> for Shape {
    type Output = ValueSym;

    fn index(&self, index: ValueName) -> &Self::Output {
        &self.values[&index]
    }
}

impl Index<TypeName> for Shape {
    type Output = TypeSym;

    fn index(&self, index: TypeName) -> &Self::Output {
        &self.types[&index]
    }
}
