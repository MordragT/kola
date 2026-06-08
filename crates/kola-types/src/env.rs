use std::{collections::HashMap, ops::Index};

use kola_resolver::symbol::{TypeSym, ValueSym};
use kola_utils::{interner::StrKey, scope::LinearScope};

use crate::types::{MonoType, PolyType, TypeVar};

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
    values: HashMap<ValueSym, PolyType>,
    types: HashMap<TypeSym, PolyType>,
}
impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_value(&mut self, sym: ValueSym, ty: PolyType) {
        self.values.insert(sym, ty);
    }

    pub fn insert_type(&mut self, sym: TypeSym, ty: PolyType) {
        self.types.insert(sym, ty);
    }

    pub fn get_value(&self, sym: ValueSym) -> Option<&PolyType> {
        self.values.get(&sym)
    }

    pub fn get_type(&self, sym: TypeSym) -> Option<&PolyType> {
        self.types.get(&sym)
    }

    pub fn merge(&mut self, other: Self) {
        for (sym, ty) in other.values {
            self.values.insert(sym, ty);
        }
        for (sym, ty) in other.types {
            self.types.insert(sym, ty);
        }
    }
}

impl Index<ValueSym> for TypeEnv {
    type Output = PolyType;

    fn index(&self, sym: ValueSym) -> &Self::Output {
        self.values
            .get(&sym)
            .expect("ValueSym not found in TypeEnvironment")
    }
}

impl Index<TypeSym> for TypeEnv {
    type Output = PolyType;

    fn index(&self, sym: TypeSym) -> &Self::Output {
        self.types
            .get(&sym)
            .expect("TypeSym not found in TypeEnvironment")
    }
}
