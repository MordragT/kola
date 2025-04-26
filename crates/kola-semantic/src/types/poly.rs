use std::fmt;

use serde::{Deserialize, Serialize};

use super::{MonoType, TypeVar, Typed};
use crate::substitute::{Substitutable, Substitution};

/// Polytype
/// Types that contains variable bound by zero or more forall
/// Polymorphic types (e.g. `∀α. α → α`, `∀α. ∀β. α → β`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Polytypes
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

    pub fn extend_free_vars(&self, buf: &mut Vec<TypeVar>) {
        self.ty.extend_type_vars(buf);
        buf.retain(|tv| !self.vars.contains(tv));
    }

    pub fn free_vars(&self) -> Vec<TypeVar> {
        let mut vars = Vec::new();
        self.extend_free_vars(&mut vars);
        vars
    }

    /// The procedure inst(σ) specializes the polytype σ by copying the term
    /// and replacing the bound type variables consistently by new monotype variables.
    pub fn instantiate(&self) -> MonoType {
        let mut ty = self.ty.clone();

        let table = self
            .vars
            .iter()
            .copied()
            .map(|tv| (tv, MonoType::variable()))
            .collect();
        let mut substitution = Substitution::new(table);

        ty.apply_mut(&mut substitution);

        ty
    }
}

impl From<MonoType> for PolyType {
    fn from(ty: MonoType) -> Self {
        Self::new(ty)
    }
}

impl fmt::Display for PolyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { vars, ty } = self;

        if !vars.is_empty() {
            write!(f, "forall ")?;

            for tv in vars {
                write!(f, "{tv}")?;
            }

            write!(f, " . ")?;
        }

        ty.fmt(f)
    }
}
