use std::ops::Index;

use crate::{
    defs::{AnyDef, Defs, ModuleDef, TypeDef, ValueDef},
    error::NameCollision,
    symbol::{LocalSyms, ModuleSym, TypeSym, ValueSym},
};
use kola_tree::node::{
    AnyName, ModuleName, ModuleNamespace, TypeName, TypeNamespace, ValueName, ValueNamespace,
};

// Idea: Create refreshner for Symbols inside ModuleShape,
// so that this can be compared to other ModuleShapes for SML style functors and such
#[derive(Debug, Clone, Default)]
pub struct ModuleShape {
    module_syms: LocalSyms<ModuleNamespace>,
    type_syms: LocalSyms<TypeNamespace>,
    value_syms: LocalSyms<ValueNamespace>,

    module_defs: Defs<ModuleNamespace>,
    type_defs: Defs<TypeNamespace>,
    value_defs: Defs<ValueNamespace>,
}

impl ModuleShape {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn contains_name(&self, name: AnyName) -> bool {
        match name {
            AnyName::Module(name) => self.module_syms.contains(name),
            AnyName::Value(name) => self.value_syms.contains(name),
            AnyName::Type(name) => self.type_syms.contains(name),
        }
    }

    // #[inline]
    // pub fn contains_sym(&self, symbol: AnySym) -> bool {
    //     match symbol {
    //         AnySym::Module(symbol) => self.modules.contains_sym(symbol),
    //         AnySym::Value(symbol) => self.values.contains_sym(symbol),
    //         AnySym::Type(symbol) => self.types.contains_sym(symbol),
    //     }
    // }

    #[inline]
    pub fn lookup_module(&self, name: ModuleName) -> Option<ModuleSym> {
        self.module_syms.get(name)
    }

    #[inline]
    pub fn lookup_type(&self, name: TypeName) -> Option<TypeSym> {
        self.type_syms.get(name)
    }

    #[inline]
    pub fn lookup_value(&self, name: ValueName) -> Option<ValueSym> {
        self.value_syms.get(name)
    }

    #[inline]
    pub fn get_module(&self, name: ModuleName) -> Option<ModuleDef> {
        self.module_syms
            .get(name)
            .and_then(|sym| self.module_defs.get(sym))
    }

    #[inline]
    pub fn get_type(&self, name: TypeName) -> Option<TypeDef> {
        self.type_syms
            .get(name)
            .and_then(|sym| self.type_defs.get(sym))
    }

    #[inline]
    pub fn get_value(&self, name: ValueName) -> Option<ValueDef> {
        self.value_syms
            .get(name)
            .and_then(|sym| self.value_defs.get(sym))
    }

    #[inline]
    pub fn get(&self, name: impl Into<AnyName>) -> Option<AnyDef> {
        match name.into() {
            AnyName::Module(name) => self.get_module(name).map(AnyDef::Module),
            AnyName::Value(name) => self.get_value(name).map(AnyDef::Value),
            AnyName::Type(name) => self.get_type(name).map(AnyDef::Type),
        }
    }

    #[inline]
    pub fn iter_modules(&self) -> impl Iterator<Item = (ModuleName, ModuleSym, ModuleDef)> {
        self.module_syms
            .iter()
            .map(|(&name, &sym)| (name, sym, self.module_defs[sym]))
    }

    #[inline]
    pub fn iter_types(&self) -> impl Iterator<Item = (TypeName, TypeSym, TypeDef)> {
        self.type_syms
            .iter()
            .map(|(&name, &sym)| (name, sym, self.type_defs[sym]))
    }

    #[inline]
    pub fn iter_values(&self) -> impl Iterator<Item = (ValueName, ValueSym, ValueDef)> {
        self.value_syms
            .iter()
            .map(|(&name, &sym)| (name, sym, self.value_defs[sym]))
    }
}

const fn name_collision(this: AnyDef, other: AnyDef) -> NameCollision {
    use kola_tree::node::NamespaceKind::*;

    let help = match (this.kind(), other.kind()) {
        (Module, Module) => "Module bindings must have distinct names from Module bindings.",
        (Module, Value) => "Module bindings must have distinct names from Value bindings.",
        (Module, Type) => "Module bindings must have distinct names from Type bindings.",
        (Value, Module) => "Value bindings must have distinct names from Module bindings.",
        (Value, Value) => "Value bindings must have distinct names from Value bindings.",
        (Value, Type) => "Value bindings must have distinct names from Type bindings.",
        (Type, Module) => "Type bindings must have distinct names from Module bindings.",
        (Type, Value) => "Type bindings must have distinct names from Value bindings.",
        (Type, Type) => "Type bindings must have distinct names from Type bindings.",
    };

    match this {
        AnyDef::Module(this) => NameCollision::module_bind(this.loc, other.location(), help),
        AnyDef::Value(this) => NameCollision::value_bind(this.loc, other.location(), help),
        AnyDef::Type(this) => NameCollision::type_bind(this.loc, other.location(), help),
    }
}

impl ModuleShape {
    pub fn insert_module(
        &mut self,
        name: ModuleName,
        sym: ModuleSym,
        def: ModuleDef,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.get(name) {
            return Err(name_collision(def.into(), bind));
        }

        self.module_syms.insert(name, sym);
        self.module_defs.insert(sym, def);
        Ok(())
    }

    pub fn insert_value(
        &mut self,
        name: ValueName,
        sym: ValueSym,
        def: ValueDef,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.get(name) {
            return Err(name_collision(def.into(), bind));
        }

        self.value_syms.insert(name, sym);
        self.value_defs.insert(sym, def);
        Ok(())
    }

    pub fn insert_type(
        &mut self,
        name: TypeName,
        sym: TypeSym,
        def: TypeDef,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.get(name) {
            return Err(name_collision(def.into(), bind));
        }

        self.type_syms.insert(name, sym);
        self.type_defs.insert(sym, def);
        Ok(())
    }
}

// impl Index<ModuleName> for LocalEnv {
//     type Output = ModuleDef;

//     fn index(&self, index: ModuleName) -> &Self::Output {
//         self.get_module(index)
//             .expect("Module name not found in local environment")
//     }
// }

// impl Index<ValueName> for LocalEnv {
//     type Output = ValueDef;

//     fn index(&self, index: ValueName) -> &Self::Output {
//         self.get_value(index)
//             .expect("Value name not found in local environment")
//     }
// }

// impl Index<TypeName> for LocalEnv {
//     type Output = TypeDef;

//     fn index(&self, index: TypeName) -> &Self::Output {
//         self.get_type(index)
//             .expect("Type name not found in local environment")
//     }
// }

impl Index<ModuleSym> for ModuleShape {
    type Output = ModuleDef;

    fn index(&self, index: ModuleSym) -> &Self::Output {
        &self.module_defs[index]
    }
}

impl Index<ValueSym> for ModuleShape {
    type Output = ValueDef;

    fn index(&self, index: ValueSym) -> &Self::Output {
        &self.value_defs[index]
    }
}

impl Index<TypeSym> for ModuleShape {
    type Output = TypeDef;

    fn index(&self, index: TypeSym) -> &Self::Output {
        &self.type_defs[index]
    }
}
