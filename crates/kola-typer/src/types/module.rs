use kola_collections::HashMap;
use kola_resolver::{
    shape::Shape,
    symbol::{ModuleSym, TypeSym, ValueSym},
};
use kola_tree::node::{ModuleName, TypeName, ValueName};
use kola_utils::interner::StrKey;

/// TODO this paragraph is completely outdated,
/// this type is only used as a side table at the moment.
///
/// Represents the interface/signature of a module, mapping exported names to their types.
///
/// Used for module signature matching, functor application, and interface comparison.
/// NOT for type lookup during type checking (use symbol tables for that). (Sure thing guess what this is currently used for)
///
/// ## Design
///
/// Stores **symbols** (references) rather than actual types for efficiency. Actual type
/// data lives in `TypeEnvironment` and is looked up by symbol. Methods like
/// `alpha_equivalent()` and `subsumes()` require a `TypeEnvironment` parameter to
/// resolve symbols for comparison and cache results.
#[derive(Debug, Clone)]
pub struct ModuleType {
    /// Nested module interfaces exported by this module
    pub modules: HashMap<ModuleName, ModuleSym>,
    /// Type definitions exported by this module
    pub types: HashMap<TypeName, TypeSym>,
    /// Value bindings exported by this module
    pub values: HashMap<ValueName, ValueSym>,
}

impl ModuleType {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            types: HashMap::new(),
            values: HashMap::new(),
        }
    }

    pub fn insert_module(&mut self, name: ModuleName, sym: ModuleSym) {
        self.modules.insert(name, sym);
    }

    pub fn insert_type(&mut self, name: TypeName, sym: TypeSym) {
        self.types.insert(name, sym);
    }

    pub fn insert_value(&mut self, name: ValueName, sym: ValueSym) {
        self.values.insert(name, sym);
    }

    pub fn get_module(&self, name: StrKey) -> Option<ModuleSym> {
        self.modules.get(&name).copied()
    }

    pub fn get_type(&self, name: StrKey) -> Option<TypeSym> {
        self.types.get(&name).copied()
    }

    pub fn get_value(&self, name: StrKey) -> Option<ValueSym> {
        self.values.get(&name).copied()
    }
}

// TODO this works for now but non exported items shouldn't be part of the module type,
// but are included in the shape.
impl From<Shape> for ModuleType {
    fn from(shape: Shape) -> Self {
        let (_functors, _module_types, modules, types, values) = shape.into_raw();

        Self {
            modules,
            types,
            values,
        }
    }
}
