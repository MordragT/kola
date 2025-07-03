use kola_collections::HashMap;
use kola_resolver::{
    shape::Shape,
    symbol::{ModuleSym, TypeSym, ValueSym},
};
use kola_tree::node::{ModuleName, TypeName, ValueName};
use kola_utils::interner::StrKey;

use crate::env::GlobalTypeEnv;

/// Represents the interface/signature of a module, mapping exported names to their types.
///
/// Used for module signature matching, functor application, and interface comparison.
/// NOT for type lookup during type checking (use symbol tables for that).
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

    // Dafuq ??
    pub fn get_module(&self, name: StrKey) -> Option<ModuleSym> {
        self.modules.get(&name).copied()
    }

    pub fn get_type(&self, name: StrKey) -> Option<TypeSym> {
        self.types.get(&name).copied()
    }

    pub fn get_value(&self, name: StrKey) -> Option<ValueSym> {
        self.values.get(&name).copied()
    }

    /// Returns true if this module type subsumes the other (contains all its requirements)
    /// In other words: self ⊇ other (self is a superset of other)
    pub fn subsumes(&self, other: &Self, env: &GlobalTypeEnv) -> bool {
        if self.modules.len() < other.modules.len()
            || self.types.len() < other.types.len()
            || self.values.len() < other.values.len()
        {
            return false;
        }

        for (name, other) in &other.modules {
            if !self
                .modules
                .get(name)
                .is_some_and(|this_module| env[*this_module].subsumes(&env[*other], env))
            {
                return false;
            }
        }

        for (name, other) in &other.types {
            if !self
                .types
                .get(name)
                .is_some_and(|this_type| env[*this_type].alpha_equivalent(&env[*other]))
            {
                return false;
            }
        }

        for (name, other) in &other.values {
            if !self
                .values
                .get(name)
                .is_some_and(|this_value| env[*this_value].alpha_equivalent(&env[*other]))
            {
                return false;
            }
        }

        true
    }

    pub fn alpha_equivalent(&self, other: &Self, env: &GlobalTypeEnv) -> bool {
        if self.modules.len() != other.modules.len()
            || self.types.len() != other.types.len()
            || self.values.len() != other.values.len()
        {
            return false;
        }

        // TODO can't I just zip them together and compare ?
        for (name, this) in &self.modules {
            if !other
                .modules
                .get(name)
                .is_some_and(|other_module| env[*this].alpha_equivalent(&env[*other_module], env))
            {
                return false;
            }
        }

        for (name, this) in &self.types {
            if !other
                .types
                .get(name)
                .is_some_and(|other_type| env[*this].alpha_equivalent(&env[*other_type]))
            {
                return false;
            }
        }

        for (name, this) in &self.values {
            if !other
                .values
                .get(name)
                .is_some_and(|other_value| env[*this].alpha_equivalent(&env[*other_value]))
            {
                return false;
            }
        }

        true
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
