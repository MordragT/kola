use std::{collections::HashMap, ops::Index};

use kola_tree::node::{AnyName, ModuleName, ModuleTypeName, TypeName, ValueName, Vis};

use crate::symbol::{AnySym, ModuleSym, ModuleTypeSym, TypeSym, ValueSym};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding<S> {
    pub sym: S,
    pub vis: Vis,
}

impl<S> Binding<S> {
    pub fn new(vis: Vis, sym: S) -> Self {
        Self { sym, vis }
    }

    pub fn map_sym<T>(self, f: impl FnOnce(S) -> T) -> Binding<T> {
        Binding {
            sym: f(self.sym),
            vis: self.vis,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct NameMap {
    module_types: HashMap<ModuleTypeName, Binding<ModuleTypeSym>>,
    modules: HashMap<ModuleName, Binding<ModuleSym>>,
    types: HashMap<TypeName, Binding<TypeSym>>,
    values: HashMap<ValueName, Binding<ValueSym>>,
}

impl NameMap {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert_module_type(
        &mut self,
        name: ModuleTypeName,
        vis: Vis,
        sym: ModuleTypeSym,
    ) -> Option<Binding<ModuleTypeSym>> {
        self.module_types.insert(name, Binding::new(vis, sym))
    }

    #[inline]
    pub fn insert_module(
        &mut self,
        name: ModuleName,
        vis: Vis,
        sym: ModuleSym,
    ) -> Option<Binding<ModuleSym>> {
        self.modules.insert(name, Binding::new(vis, sym))
    }

    #[inline]
    pub fn insert_type(
        &mut self,
        name: TypeName,
        vis: Vis,
        sym: TypeSym,
    ) -> Option<Binding<TypeSym>> {
        self.types.insert(name, Binding::new(vis, sym))
    }

    #[inline]
    pub fn insert_value(
        &mut self,
        name: ValueName,
        vis: Vis,
        sym: ValueSym,
    ) -> Option<Binding<ValueSym>> {
        self.values.insert(name, Binding::new(vis, sym))
    }

    #[inline]
    pub fn contains_name(&self, name: AnyName) -> bool {
        match name {
            AnyName::ModuleType(name) => self.module_types.contains_key(&name),
            AnyName::Module(name) => self.modules.contains_key(&name),
            AnyName::Type(name) => self.types.contains_key(&name),
            AnyName::Value(name) => self.values.contains_key(&name),
            _ => false,
        }
    }

    #[inline]
    pub fn get_module_type(&self, name: ModuleTypeName) -> Option<Binding<ModuleTypeSym>> {
        self.module_types.get(&name).copied()
    }

    #[inline]
    pub fn get_module(&self, name: ModuleName) -> Option<Binding<ModuleSym>> {
        self.modules.get(&name).copied()
    }

    #[inline]
    pub fn get_type(&self, name: TypeName) -> Option<Binding<TypeSym>> {
        self.types.get(&name).copied()
    }

    #[inline]
    pub fn get_value(&self, name: ValueName) -> Option<Binding<ValueSym>> {
        self.values.get(&name).copied()
    }

    #[inline]
    pub fn get(&self, name: impl Into<AnyName>) -> Option<Binding<AnySym>> {
        match name.into() {
            AnyName::ModuleType(name) => self
                .get_module_type(name)
                .map(|b| b.map_sym(AnySym::ModuleType)),
            AnyName::Module(name) => self.get_module(name).map(|b| b.map_sym(AnySym::Module)),
            AnyName::Type(name) => self.get_type(name).map(|b| b.map_sym(AnySym::Type)),
            AnyName::Value(name) => self.get_value(name).map(|b| b.map_sym(AnySym::Value)),
            _ => None,
        }
    }

    #[inline]
    pub fn iter_module_types(&self) -> impl Iterator<Item = (ModuleTypeName, Vis, ModuleTypeSym)> {
        self.module_types
            .iter()
            .map(|(&name, binding)| (name, binding.vis, binding.sym))
    }

    #[inline]
    pub fn iter_modules(&self) -> impl Iterator<Item = (ModuleName, Vis, ModuleSym)> {
        self.modules
            .iter()
            .map(|(&name, binding)| (name, binding.vis, binding.sym))
    }

    #[inline]
    pub fn iter_types(&self) -> impl Iterator<Item = (TypeName, Vis, TypeSym)> {
        self.types
            .iter()
            .map(|(&name, binding)| (name, binding.vis, binding.sym))
    }

    #[inline]
    pub fn iter_values(&self) -> impl Iterator<Item = (ValueName, Vis, ValueSym)> {
        self.values
            .iter()
            .map(|(&name, binding)| (name, binding.vis, binding.sym))
    }

    pub fn into_raw(
        self,
    ) -> (
        HashMap<ModuleTypeName, Binding<ModuleTypeSym>>,
        HashMap<ModuleName, Binding<ModuleSym>>,
        HashMap<TypeName, Binding<TypeSym>>,
        HashMap<ValueName, Binding<ValueSym>>,
    ) {
        (self.module_types, self.modules, self.types, self.values)
    }
}

impl Index<ModuleTypeName> for NameMap {
    type Output = Binding<ModuleTypeSym>;

    fn index(&self, index: ModuleTypeName) -> &Self::Output {
        &self.module_types[&index]
    }
}

impl Index<ModuleName> for NameMap {
    type Output = Binding<ModuleSym>;

    fn index(&self, index: ModuleName) -> &Self::Output {
        &self.modules[&index]
    }
}

impl Index<TypeName> for NameMap {
    type Output = Binding<TypeSym>;

    fn index(&self, index: TypeName) -> &Self::Output {
        &self.types[&index]
    }
}

impl Index<ValueName> for NameMap {
    type Output = Binding<ValueSym>;

    fn index(&self, index: ValueName) -> &Self::Output {
        &self.values[&index]
    }
}
