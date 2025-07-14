use std::ops::Index;

use crate::symbol::{
    AnySym, EffectSym, FunctorSym, ModuleSym, ModuleTypeSym, Substitute, TypeSym, ValueSym, merge6,
};
use kola_collections::HashMap;
use kola_tree::node::{
    AnyName, EffectName, EffectNamespace, FunctorName, FunctorNamespace, ModuleName,
    ModuleNamespace, ModuleTypeName, ModuleTypeNamespace, TypeName, TypeNamespace, ValueName,
    ValueNamespace,
};

// TODO this is more of a symbol table than a shape, consider renaming
#[derive(Debug, Clone, Default)]
pub struct Shape {
    functors: HashMap<FunctorName, FunctorSym>,
    module_types: HashMap<ModuleTypeName, ModuleTypeSym>,
    modules: HashMap<ModuleName, ModuleSym>,
    effects: HashMap<EffectName, EffectSym>,
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
    pub fn insert_effect(&mut self, name: EffectName, sym: EffectSym) -> Option<EffectSym> {
        self.effects.insert(name, sym)
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
            AnyName::Effect(name) => self.effects.contains_key(&name),
            AnyName::Type(name) => self.types.contains_key(&name),
            AnyName::Value(name) => self.values.contains_key(&name),
            _ => false,
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
    pub fn get_effect(&self, name: EffectName) -> Option<EffectSym> {
        self.effects.get(&name).copied()
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
            AnyName::Effect(name) => self.get_effect(name).map(AnySym::Effect),
            AnyName::Type(name) => self.get_type(name).map(AnySym::Type),
            AnyName::Value(name) => self.get_value(name).map(AnySym::Value),
            _ => None,
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
    pub fn iter_effects(&self) -> impl Iterator<Item = (EffectName, EffectSym)> {
        self.effects.iter().map(|(&name, &sym)| (name, sym))
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
        HashMap<EffectName, EffectSym>,
        HashMap<TypeName, TypeSym>,
        HashMap<ValueName, ValueSym>,
    ) {
        (
            self.functors,
            self.module_types,
            self.modules,
            self.effects,
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

impl Index<EffectName> for Shape {
    type Output = EffectSym;

    fn index(&self, index: EffectName) -> &Self::Output {
        &self.effects[&index]
    }
}

impl Index<TypeName> for Shape {
    type Output = TypeSym;

    fn index(&self, index: TypeName) -> &Self::Output {
        &self.types[&index]
    }
}

impl Index<ValueName> for Shape {
    type Output = ValueSym;

    fn index(&self, index: ValueName) -> &Self::Output {
        &self.values[&index]
    }
}

impl Substitute for Shape {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        let functors = self.functors.try_subst(s);
        let module_types = self.module_types.try_subst(s);
        let modules = self.modules.try_subst(s);
        let effects = self.effects.try_subst(s);
        let types = self.types.try_subst(s);
        let values = self.values.try_subst(s);

        merge6(
            functors,
            || self.functors.clone(),
            module_types,
            || self.module_types.clone(),
            modules,
            || self.modules.clone(),
            effects,
            || self.effects.clone(),
            types,
            || self.types.clone(),
            values,
            || self.values.clone(),
        )
        .map(
            |(functors, module_types, modules, effects, types, values)| Self {
                functors,
                module_types,
                modules,
                effects,
                types,
                values,
            },
        )
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        self.functors.subst_mut(s);
        self.module_types.subst_mut(s);
        self.modules.subst_mut(s);
        self.effects.subst_mut(s);
        self.types.subst_mut(s);
        self.values.subst_mut(s);
    }
}
