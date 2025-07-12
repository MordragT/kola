use std::{collections::HashMap, ops::Index};

use indexmap::IndexMap;
use kola_resolver::{
    defs::{EffectTypeDef, TypeDef, ValueDef},
    info::ModuleInfo,
    symbol::{EffectSym, ModuleSym, TypeSym, ValueSym},
};
use kola_tree::node::{self, NodeId};
use kola_utils::{interner::StrKey, scope::LinearScope};

use crate::types::{ModuleType, MonoType, PolyType, RowType, TypeClass, TypeVar};

pub type TypeClassEnv = IndexMap<TypeVar, Vec<TypeClass>>;

pub trait BoundVars {
    /// Extends the given vector with the type variables that are bound in this type.
    fn extend_bound_vars(&self, vars: &mut Vec<TypeVar>);

    /// Returns the type variables that are bound in this type.
    fn bound_vars(&self) -> Vec<TypeVar> {
        let mut vars = Vec::new();
        self.extend_bound_vars(&mut vars);
        vars.sort_unstable();
        vars.dedup();
        vars
    }
}

pub type LocalTypeEnv = LinearScope<StrKey, MonoType>;

#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    values: HashMap<ValueSym, (ValueDef, PolyType)>,
    types: HashMap<TypeSym, (TypeDef, PolyType)>,
    effects: HashMap<EffectSym, (EffectTypeDef, RowType)>,
    modules: HashMap<ModuleSym, (ModuleInfo, ModuleType)>,
}
impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_value(&mut self, sym: ValueSym, def: ValueDef, ty: PolyType) {
        self.values.insert(sym, (def, ty));
    }

    pub fn insert_type(&mut self, sym: TypeSym, def: TypeDef, ty: PolyType) {
        self.types.insert(sym, (def, ty));
    }

    pub fn insert_effect(&mut self, sym: EffectSym, def: EffectTypeDef, ty: RowType) {
        self.effects.insert(sym, (def, ty));
    }

    pub fn insert_module(&mut self, sym: ModuleSym, info: ModuleInfo, ty: ModuleType) {
        self.modules.insert(sym, (info, ty));
    }

    pub fn get_value(&self, sym: ValueSym) -> Option<(ValueDef, &PolyType)> {
        self.values.get(&sym).map(|(def, ty)| (*def, ty))
    }

    pub fn get_type(&self, sym: TypeSym) -> Option<(TypeDef, &PolyType)> {
        self.types.get(&sym).map(|(def, ty)| (*def, ty))
    }

    pub fn get_effect(&self, sym: EffectSym) -> Option<(EffectTypeDef, &RowType)> {
        self.effects.get(&sym).map(|(def, ty)| (*def, ty))
    }

    pub fn get_module(&self, sym: ModuleSym) -> Option<(ModuleInfo, &ModuleType)> {
        self.modules.get(&sym).map(|(info, ty)| (*info, ty))
    }

    pub fn merge(&mut self, other: Self) {
        for (sym, (def, ty)) in other.values {
            self.values.insert(sym, (def, ty));
        }
        for (sym, (def, ty)) in other.types {
            self.types.insert(sym, (def, ty));
        }
        for (sym, (def, ty)) in other.effects {
            self.effects.insert(sym, (def, ty));
        }
        for (sym, (info, ty)) in other.modules {
            self.modules.insert(sym, (info, ty));
        }
    }
}

impl Index<ValueSym> for TypeEnv {
    type Output = (ValueDef, PolyType);

    fn index(&self, sym: ValueSym) -> &Self::Output {
        self.values
            .get(&sym)
            .expect("ValueSym not found in TypeEnvironment")
    }
}

impl Index<TypeSym> for TypeEnv {
    type Output = (TypeDef, PolyType);

    fn index(&self, sym: TypeSym) -> &Self::Output {
        self.types
            .get(&sym)
            .expect("TypeSym not found in TypeEnvironment")
    }
}

impl Index<EffectSym> for TypeEnv {
    type Output = (EffectTypeDef, RowType);

    fn index(&self, sym: EffectSym) -> &Self::Output {
        self.effects
            .get(&sym)
            .expect("EffectSym not found in TypeEnvironment")
    }
}

impl Index<ModuleSym> for TypeEnv {
    type Output = (ModuleInfo, ModuleType);

    fn index(&self, sym: ModuleSym) -> &Self::Output {
        self.modules
            .get(&sym)
            .expect("ModuleSym not found in TypeEnvironment")
    }
}
