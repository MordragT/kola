use std::ops::Index;

use crate::{
    def::{AnyDef, Defs, ModuleDef, TypeDef, ValueDef},
    error::NameCollision,
    symbol::{LocalSyms, ModuleSym, TypeSym, ValueSym},
};
use kola_span::Loc;
use kola_tree::node::{
    AnyName, ModuleName, ModuleNamespace, TypeName, TypeNamespace, ValueName, ValueNamespace,
};

#[derive(Debug, Clone)]
pub struct LocalEnv {
    loc: Loc,

    module_syms: LocalSyms<ModuleNamespace>,
    type_syms: LocalSyms<TypeNamespace>,
    value_syms: LocalSyms<ValueNamespace>,

    module_defs: Defs<ModuleNamespace>,
    type_defs: Defs<TypeNamespace>,
    value_defs: Defs<ValueNamespace>,
}

impl LocalEnv {
    pub fn new(loc: Loc) -> Self {
        Self {
            loc,
            module_syms: LocalSyms::new(),
            type_syms: LocalSyms::new(),
            value_syms: LocalSyms::new(),
            module_defs: Defs::new(),
            type_defs: Defs::new(),
            value_defs: Defs::new(),
        }
    }

    #[inline]
    pub fn loc(&self) -> Loc {
        self.loc
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

impl LocalEnv {
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

impl Index<ModuleSym> for LocalEnv {
    type Output = ModuleDef;

    fn index(&self, index: ModuleSym) -> &Self::Output {
        &self.module_defs[index]
    }
}

impl Index<ValueSym> for LocalEnv {
    type Output = ValueDef;

    fn index(&self, index: ValueSym) -> &Self::Output {
        &self.value_defs[index]
    }
}

impl Index<TypeSym> for LocalEnv {
    type Output = TypeDef;

    fn index(&self, index: TypeSym) -> &Self::Output {
        &self.type_defs[index]
    }
}

// #[derive(Debug, Clone, Default)]
// pub struct GlobalEnv {
//     pub modules: IndexMap<ModuleSym, Rc<LocalEnv>>,
// }

// impl GlobalEnv {
//     pub fn new() -> Self {
//         Self::default()
//     }

//     pub fn insert_module(&mut self, sym: ModuleSym, env: LocalEnv) {
//         self.modules.insert(sym, Rc::new(env));
//     }

//     pub fn get_module(&self, sym: ModuleSym) -> Option<Rc<LocalEnv>> {
//         self.modules.get(&sym).cloned()
//     }

//     pub fn contains_module(&self, sym: ModuleSym) -> bool {
//         self.modules.contains_key(&sym)
//     }
// }

// impl Index<ModuleSym> for GlobalEnv {
//     type Output = Rc<LocalEnv>;

//     fn index(&self, index: ModuleSym) -> &Self::Output {
//         self.modules
//             .get(&index)
//             .expect("Module symbol not found in global environment")
//     }
// }

// TODO Idea: create a GlobalEnv that contains LocalEnv for each module
// Do not store LocalEnv inside the ModuleScope
