use crate::syntax::ast::Ident;

use super::{
    error::InferError,
    types::{PolyType, TypeVar},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Default for Scopes {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }
}

impl Scopes {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.scopes.clear();
        self.scopes.push(Scope::new())
    }

    pub fn get(&self, ident: &Ident) -> Option<&PolyType> {
        self.scopes.iter().rev().find_map(|s| s.get(ident))
    }

    pub fn try_get(&self, ident: &Ident) -> Result<&PolyType, InferError> {
        self.get(ident).ok_or(InferError::Unbound(ident.clone()))
    }

    pub fn insert(&mut self, ident: Ident, ty: PolyType) {
        self.scopes
            .last_mut()
            .expect("no active scope")
            .insert(ident, ty);
    }

    pub fn bound_vars(&self) -> Vec<TypeVar> {
        let mut vars = self
            .scopes
            .iter()
            .flat_map(|s| {
                s.bindings
                    .iter()
                    .flat_map(|(_, pt)| pt.bound_vars().iter().copied())
            })
            .collect::<Vec<_>>();

        vars.sort_unstable();
        vars.dedup();

        vars
    }

    // TODO instead of manual enter exit maybe callback for safer use ?

    pub fn enter(&mut self) {
        self.scopes.push(Scope::new())
    }

    pub fn exit(&mut self) {
        self.scopes.pop().expect("no active scope");
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Scope {
    bindings: Vec<(Ident, PolyType)>, // Indexmap ??
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, ident: &Ident) -> Option<&PolyType> {
        self.bindings
            .iter()
            .find_map(|(i, t)| if i == ident { Some(t) } else { None })
    }

    pub fn insert(&mut self, ident: Ident, ty: PolyType) {
        self.bindings.push((ident, ty));
    }
}
