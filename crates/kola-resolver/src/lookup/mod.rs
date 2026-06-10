mod module;
mod module_ty;
mod ty;
mod value;

use std::collections::HashMap;

pub use module::{ModuleLookup, lookup_modules};
pub use module_ty::{ModuleTypeAnnotLookup, ModuleTypeLookup, lookup_module_types};
pub use ty::{TypeAnnotLookup, TypeLookup, lookup_types};
pub use value::{ValueLookup, lookup_values};

use crate::symbol::{AnySym, Substitute, merge6};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Lookups {
    pub module_types: Vec<ModuleTypeLookup>,
    pub module_type_annots: Vec<ModuleTypeAnnotLookup>,
    pub modules: Vec<ModuleLookup>,
    pub types: Vec<TypeLookup>,
    pub type_annots: Vec<TypeAnnotLookup>,
    pub values: Vec<ValueLookup>,
}

impl Lookups {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn append(&mut self, other: Self) {
        self.module_types.extend(other.module_types);
        self.module_type_annots.extend(other.module_type_annots);
        self.modules.extend(other.modules);
        self.types.extend(other.types);
        self.type_annots.extend(other.type_annots);
        self.values.extend(other.values);
    }

    #[inline]
    pub fn insert_module_type(&mut self, type_ref: ModuleTypeLookup) {
        self.module_types.push(type_ref);
    }

    #[inline]
    pub fn insert_module_type_annot(&mut self, type_ref: ModuleTypeAnnotLookup) {
        self.module_type_annots.push(type_ref);
    }

    #[inline]
    pub fn insert_module(&mut self, module_ref: ModuleLookup) {
        self.modules.push(module_ref);
    }

    #[inline]
    pub fn insert_type(&mut self, type_ref: TypeLookup) {
        self.types.push(type_ref);
    }

    #[inline]
    pub fn insert_type_annot(&mut self, type_ref: TypeAnnotLookup) {
        self.type_annots.push(type_ref);
    }

    #[inline]
    pub fn insert_value(&mut self, value_ref: ValueLookup) {
        self.values.push(value_ref);
    }

    #[inline]
    pub fn module_types(&self) -> &[ModuleTypeLookup] {
        &self.module_types
    }

    #[inline]
    pub fn module_type_annots(&self) -> &[ModuleTypeAnnotLookup] {
        &self.module_type_annots
    }

    #[inline]
    pub fn modules(&self) -> &[ModuleLookup] {
        &self.modules
    }

    #[inline]
    pub fn types(&self) -> &[TypeLookup] {
        &self.types
    }

    #[inline]
    pub fn type_annots(&self) -> &[TypeAnnotLookup] {
        &self.type_annots
    }

    #[inline]
    pub fn values(&self) -> &[ValueLookup] {
        &self.values
    }
}

impl Substitute for Lookups {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let Self {
            module_types,
            module_type_annots,
            modules,
            types,
            type_annots,
            values,
        } = self;

        let module_types_opt = module_types.try_subst(s);
        let module_type_annots_opt = module_type_annots.try_subst(s);
        let modules_opt = modules.try_subst(s);
        let types_opt = types.try_subst(s);
        let type_annots_opt = type_annots.try_subst(s);
        let values_opt = values.try_subst(s);

        merge6(
            module_types_opt,
            || module_types.clone(),
            module_type_annots_opt,
            || module_type_annots.clone(),
            modules_opt,
            || modules.clone(),
            types_opt,
            || types.clone(),
            type_annots_opt,
            || type_annots.clone(),
            values_opt,
            || values.clone(),
        )
        .map(
            |(module_type_binds, module_types, modules, type_binds, types, values)| Self {
                module_types: module_type_binds,
                module_type_annots: module_types,
                modules,
                types: type_binds,
                type_annots: types,
                values,
            },
        )
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        self.module_types.subst_mut(s);
        self.module_type_annots.subst_mut(s);
        self.modules.subst_mut(s);
        self.types.subst_mut(s);
        self.type_annots.subst_mut(s);
        self.values.subst_mut(s);
    }
}
