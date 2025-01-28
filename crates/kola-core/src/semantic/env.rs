use indexmap::IndexMap;
use kola_tree::Symbol;

use super::{
    error::SemanticError,
    types::{Kind, PolyType, TypeVar},
};

pub type KindEnv = IndexMap<TypeVar, Vec<Kind>>;

/// A type environment maps program identifiers to their polymorphic types.
///
/// Type environments are implemented as a stack where each
/// frame holds the bindings for the identifiers declared in a particular
/// lexical block.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TypeEnv {
    // An external environment if one is provided
    // external: ...
    /// An optional parent environment.
    parent: Option<Box<Self>>,
    /// Uses an `IndexMap` to ensure that the order that the bindings were defined in matches
    /// the iteration order
    scope: IndexMap<Symbol, PolyType>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.scope.clear();
    }

    pub fn lookup(&self, name: &Symbol) -> Option<&PolyType> {
        if let Some(t) = self.scope.get(name) {
            Some(t)
        } else if let Some(t) = self.parent.as_ref().and_then(|env| env.lookup(name)) {
            Some(t)
        } else {
            None
        }
    }

    pub fn try_lookup(&self, name: &Symbol) -> Result<&PolyType, SemanticError> {
        self.lookup(name)
            .ok_or(SemanticError::Unbound(name.clone()))
    }

    pub fn insert(&mut self, name: Symbol, t: PolyType) {
        self.scope.insert(name, t);
    }

    fn extend_bound_vars(&self, vars: &mut Vec<TypeVar>) {
        vars.extend(self.scope.values().flat_map(PolyType::bound_vars));

        if let Some(parent) = &self.parent {
            parent.extend_bound_vars(vars);
        }
    }

    pub fn bound_vars(&self) -> Vec<TypeVar> {
        let mut vars = Vec::new();

        self.extend_bound_vars(&mut vars);

        vars.sort_unstable();
        vars.dedup();

        vars
    }

    pub fn enter(&mut self) {
        let parent = std::mem::replace(self, Self::new());
        self.parent = Some(Box::new(parent));
    }

    pub fn exit(&mut self) -> Self {
        let mut parent = self.parent.take().expect("no active scope");
        std::mem::swap(self, &mut parent);
        *parent
    }
}
