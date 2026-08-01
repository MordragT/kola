use kola_print::prelude::OwoColorize;
use std::{collections::HashMap, fmt};

use crate::{
    env::BoundVars,
    types::{MonoType, TypeVar},
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Substitution {
    table: HashMap<TypeVar, MonoType>,
    /// Path compression is a technique commonly used in Union-Find data structures.
    /// We apply it here so that whenever a chain of substitutions is traversed,
    /// each variable is updated to point to its ultimate value. For example, the
    /// chain:
    ///
    /// `t0 ↦ t1`, `t1 ↦ t2`, and `t2 ↦ int`
    ///
    /// becomes
    ///
    /// `t0 ↦ int`, `t1 ↦ int`, and `t2 ↦ int`
    ///
    /// Rather than updating the actual mappings,
    /// this cache maintains these compressed mappings.
    cache: HashMap<TypeVar, MonoType>,
}

impl fmt::Display for Substitution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (var, ty) in &self.table {
            writeln!(f, "{var}\t{}", ty.green())?;
        }

        Ok(())
    }
}

impl Substitution {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn unit(var: TypeVar, ty: MonoType) -> Self {
        let mut table = HashMap::new();
        table.insert(var, ty);
        Self {
            table,
            cache: HashMap::new(),
        }
    }

    pub fn new(table: HashMap<TypeVar, MonoType>) -> Self {
        Self {
            table,
            cache: HashMap::new(),
        }
    }

    pub fn get(&self, tv: &TypeVar) -> Option<&MonoType> {
        self.cache.get(tv).or_else(|| self.table.get(tv))
    }

    pub fn contains(&self, tv: &TypeVar) -> bool {
        self.cache.contains_key(tv) || self.table.contains_key(tv)
    }

    pub fn insert(&mut self, tv: TypeVar, ty: MonoType) -> Option<MonoType> {
        self.table.insert(tv, ty)
    }

    pub fn cache(&mut self, tv: TypeVar, ty: &MonoType) {
        self.cache
            .entry(tv)
            .and_modify(|stored| {
                if stored != ty {
                    *stored = ty.clone();
                }
            })
            .or_insert_with(|| ty.clone());
    }

    pub fn is_empty(&self) -> bool {
        self.table.is_empty() && self.cache.is_empty()
    }

    pub fn clear(&mut self) {
        self.table.clear();
        self.cache.clear();
    }
}

impl BoundVars for Substitution {
    fn extend_bound_vars(&self, vars: &mut Vec<TypeVar>) {
        vars.extend(self.table.keys());
    }
}
