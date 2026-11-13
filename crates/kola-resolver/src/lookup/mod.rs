mod module;
mod module_ty;
mod ty;
mod value;


pub use module::{ModuleLookup, lookup_modules};
pub use module_ty::{ModuleTypeAnnotLookup, ModuleTypeLookup, lookup_module_types};
pub use ty::{TypeAnnotLookup, TypeLookup, lookup_types};
pub use value::{ValueLookup, lookup_values};


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

