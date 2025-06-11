use kola_utils::{interner::StrKey, scope::LinearScope};

use crate::types::{PolyType, TypeVar};

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

/// A type scope maps program identifiers to their polymorphic types.
///
/// Type scope are implemented as a stack where each
/// frame holds the bindings for the identifiers declared in a particular
/// lexical block.
pub type TypeScope = LinearScope<StrKey, PolyType>;

impl BoundVars for TypeScope {
    fn extend_bound_vars(&self, vars: &mut Vec<TypeVar>) {
        vars.extend(self.values().flat_map(PolyType::bound_vars));
    }
}
