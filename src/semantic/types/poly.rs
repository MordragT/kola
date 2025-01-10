use std::collections::HashMap;

use crate::semantic::Substitutable;

use super::{MonoType, TypeVar};

/// Polytype
/// Types that contains variable bound by zero or more forall
/// Polymorphic types (e.g. `∀α. α → α`, `∀α. ∀β. α → β`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Polytypes
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PolyType {
    pub(super) vars: Vec<TypeVar>,
    pub(super) ty: MonoType,
}

impl PolyType {
    pub fn new(ty: MonoType) -> Self {
        Self {
            vars: Vec::new(),
            ty,
        }
    }

    pub fn bound_vars(&self) -> &Vec<TypeVar> {
        &self.vars
    }

    // pub fn free_type_vars(&self, buf: &mut Vec<TypeVar>) {
    //     self.ty.type_vars(buf);
    //     buf.retain(|tv| !self.vars.contains(tv));
    // }

    /// The procedure inst(σ) specializes the polytype σ by copying the term
    /// and replacing the bound type variables consistently by new monotype variables.
    pub fn instantiate(&self) -> MonoType {
        let mut ty = self.ty.clone();

        let mut substitution = self
            .vars
            .iter()
            .copied()
            .map(|tv| (tv, MonoType::variable()))
            .collect();

        ty.apply_mut(&mut substitution, &mut HashMap::new());

        ty
    }
}

impl From<MonoType> for PolyType {
    fn from(ty: MonoType) -> Self {
        Self::new(ty)
    }
}
