use std::{borrow::Cow, ops::Index};

use crate::symbol::{AnySym, FunctorSym, ModuleSym, ModuleTypeSym, Substitute, TypeSym, ValueSym};
use kola_collections::HashMap;
use kola_tree::node::{
    AnyName, FunctorName, FunctorNamespace, ModuleName, ModuleNamespace, ModuleTypeName,
    ModuleTypeNamespace, TypeName, TypeNamespace, ValueName, ValueNamespace,
};

// TODO this is more of a symbol table than a shape, consider renaming
#[derive(Debug, Clone, Default)]
pub struct Shape {
    functors: HashMap<FunctorName, FunctorSym>,
    module_types: HashMap<ModuleTypeName, ModuleTypeSym>,
    modules: HashMap<ModuleName, ModuleSym>,
    types: HashMap<TypeName, TypeSym>,
    values: HashMap<ValueName, ValueSym>,
}

impl Shape {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert_functor(&mut self, name: FunctorName, sym: FunctorSym) -> Option<FunctorSym> {
        self.functors.insert(name, sym)
    }

    #[inline]
    pub fn insert_module_type(
        &mut self,
        name: ModuleTypeName,
        sym: ModuleTypeSym,
    ) -> Option<ModuleTypeSym> {
        self.module_types.insert(name, sym)
    }

    #[inline]
    pub fn insert_module(&mut self, name: ModuleName, sym: ModuleSym) -> Option<ModuleSym> {
        self.modules.insert(name, sym)
    }

    #[inline]
    pub fn insert_type(&mut self, name: TypeName, sym: TypeSym) -> Option<TypeSym> {
        self.types.insert(name, sym)
    }

    #[inline]
    pub fn insert_value(&mut self, name: ValueName, sym: ValueSym) -> Option<ValueSym> {
        self.values.insert(name, sym)
    }

    #[inline]
    pub fn contains_name(&self, name: AnyName) -> bool {
        match name {
            AnyName::Functor(name) => self.functors.contains_key(&name),
            AnyName::ModuleType(name) => self.module_types.contains_key(&name),
            AnyName::Module(name) => self.modules.contains_key(&name),
            AnyName::Value(name) => self.values.contains_key(&name),
            AnyName::Type(name) => self.types.contains_key(&name),
        }
    }

    #[inline]
    pub fn get_functor(&self, name: FunctorName) -> Option<FunctorSym> {
        self.functors.get(&name).copied()
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
            AnyName::Functor(name) => self.get_functor(name).map(AnySym::Functor),
            AnyName::ModuleType(name) => self.get_module_type(name).map(AnySym::ModuleType),
            AnyName::Module(name) => self.get_module(name).map(AnySym::Module),
            AnyName::Value(name) => self.get_value(name).map(AnySym::Value),
            AnyName::Type(name) => self.get_type(name).map(AnySym::Type),
        }
    }

    #[inline]
    pub fn iter_functors(&self) -> impl Iterator<Item = (FunctorName, FunctorSym)> {
        self.functors.iter().map(|(&name, &sym)| (name, sym))
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
        HashMap<FunctorName, FunctorSym>,
        HashMap<ModuleTypeName, ModuleTypeSym>,
        HashMap<ModuleName, ModuleSym>,
        HashMap<TypeName, TypeSym>,
        HashMap<ValueName, ValueSym>,
    ) {
        (
            self.functors,
            self.module_types,
            self.modules,
            self.types,
            self.values,
        )
    }
}

impl Index<FunctorName> for Shape {
    type Output = FunctorSym;

    fn index(&self, index: FunctorName) -> &Self::Output {
        &self.functors[&index]
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

impl Substitute<FunctorNamespace> for Shape {
    fn try_substitute(&self, from: FunctorSym, to: FunctorSym) -> Option<Self>
    where
        Self: Sized,
    {
        if let Some(functors) = self.functors.try_substitute(from, to) {
            Some(Self {
                functors,
                ..self.clone()
            })
        } else {
            None
        }
    }

    fn substitute_mut(&mut self, from: FunctorSym, to: FunctorSym)
    where
        Self: Sized,
    {
        self.functors.substitute_mut(from, to);
    }
}

impl Substitute<ModuleTypeNamespace> for Shape {
    fn try_substitute(&self, from: ModuleTypeSym, to: ModuleTypeSym) -> Option<Self>
    where
        Self: Sized,
    {
        if let Some(module_types) = self.module_types.try_substitute(from, to) {
            Some(Self {
                module_types,
                ..self.clone()
            })
        } else {
            None
        }
    }

    fn substitute_mut(&mut self, from: ModuleTypeSym, to: ModuleTypeSym)
    where
        Self: Sized,
    {
        self.module_types.substitute_mut(from, to);
    }
}

impl Substitute<ModuleNamespace> for Shape {
    fn try_substitute(&self, from: ModuleSym, to: ModuleSym) -> Option<Self>
    where
        Self: Sized,
    {
        if let Some(modules) = self.modules.try_substitute(from, to) {
            Some(Self {
                modules,
                ..self.clone()
            })
        } else {
            None
        }
    }

    fn substitute_mut(&mut self, from: ModuleSym, to: ModuleSym)
    where
        Self: Sized,
    {
        self.modules.substitute_mut(from, to);
    }
}

impl Substitute<TypeNamespace> for Shape {
    fn try_substitute(&self, from: TypeSym, to: TypeSym) -> Option<Self>
    where
        Self: Sized,
    {
        if let Some(types) = self.types.try_substitute(from, to) {
            Some(Self {
                types,
                ..self.clone()
            })
        } else {
            None
        }
    }

    fn substitute_mut(&mut self, from: TypeSym, to: TypeSym)
    where
        Self: Sized,
    {
        self.types.substitute_mut(from, to);
    }
}

impl Substitute<ValueNamespace> for Shape {
    fn try_substitute(&self, from: ValueSym, to: ValueSym) -> Option<Self>
    where
        Self: Sized,
    {
        if let Some(values) = self.values.try_substitute(from, to) {
            Some(Self {
                values,
                ..self.clone()
            })
        } else {
            None
        }
    }

    fn substitute_mut(&mut self, from: ValueSym, to: ValueSym)
    where
        Self: Sized,
    {
        self.values.substitute_mut(from, to);
    }
}
