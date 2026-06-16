use std::{collections::HashMap, ops::Index};

use crate::symbol::{
    AnySym, FunctorSym, ModuleSym, ModuleTypeSym, Substitute, TypeSym, ValueSym, merge5,
};
use kola_tree::node::{AnyName, FunctorName, ModuleName, ModuleTypeName, TypeName, ValueName, Vis};

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

impl<S> Substitute for Binding<S>
where
    S: Substitute,
{
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        if let Some(sym) = self.sym.try_subst(s) {
            Some(Self::new(self.vis, sym))
        } else {
            None
        }
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        self.sym.subst_mut(s);
    }
}

#[derive(Debug, Clone, Default)]
pub struct NameMap {
    functors: HashMap<FunctorName, Binding<FunctorSym>>,
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
    pub fn insert_functor(
        &mut self,
        name: FunctorName,
        vis: Vis,
        sym: FunctorSym,
    ) -> Option<Binding<FunctorSym>> {
        self.functors.insert(name, Binding::new(vis, sym))
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
            AnyName::Functor(name) => self.functors.contains_key(&name),
            AnyName::ModuleType(name) => self.module_types.contains_key(&name),
            AnyName::Module(name) => self.modules.contains_key(&name),
            AnyName::Type(name) => self.types.contains_key(&name),
            AnyName::Value(name) => self.values.contains_key(&name),
            _ => false,
        }
    }

    #[inline]
    pub fn get_functor(&self, name: FunctorName) -> Option<Binding<FunctorSym>> {
        self.functors.get(&name).copied()
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
            AnyName::Functor(name) => self.get_functor(name).map(|b| b.map_sym(AnySym::Functor)),
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
    pub fn iter_functors(&self) -> impl Iterator<Item = (FunctorName, Vis, FunctorSym)> {
        self.functors
            .iter()
            .map(|(&name, binding)| (name, binding.vis, binding.sym))
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
        HashMap<FunctorName, Binding<FunctorSym>>,
        HashMap<ModuleTypeName, Binding<ModuleTypeSym>>,
        HashMap<ModuleName, Binding<ModuleSym>>,
        HashMap<TypeName, Binding<TypeSym>>,
        HashMap<ValueName, Binding<ValueSym>>,
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

impl Index<FunctorName> for NameMap {
    type Output = Binding<FunctorSym>;

    fn index(&self, index: FunctorName) -> &Self::Output {
        &self.functors[&index]
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

impl Substitute for NameMap {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        let functors = self.functors.try_subst(s);
        let module_types = self.module_types.try_subst(s);
        let modules = self.modules.try_subst(s);
        let types = self.types.try_subst(s);
        let values = self.values.try_subst(s);

        merge5(
            functors,
            || self.functors.clone(),
            module_types,
            || self.module_types.clone(),
            modules,
            || self.modules.clone(),
            types,
            || self.types.clone(),
            values,
            || self.values.clone(),
        )
        .map(|(functors, module_types, modules, types, values)| Self {
            functors,
            module_types,
            modules,
            types,
            values,
        })
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        self.functors.subst_mut(s);
        self.module_types.subst_mut(s);
        self.modules.subst_mut(s);
        self.types.subst_mut(s);
        self.values.subst_mut(s);
    }
}
