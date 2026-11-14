mod module;
mod module_ty;
mod ty;
mod value;

pub use module::{ModuleLookup, lookup_modules};
pub use module_ty::order_module_types;
pub use ty::order_types;
pub use value::order_values;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Lookups {
    pub modules: Vec<ModuleLookup>,
}

impl Lookups {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn append(&mut self, other: Self) {
        self.modules.extend(other.modules);
    }

    #[inline]
    pub fn insert_module(&mut self, module_ref: ModuleLookup) {
        self.modules.push(module_ref);
    }

    #[inline]
    pub fn modules(&self) -> &[ModuleLookup] {
        &self.modules
    }
}
