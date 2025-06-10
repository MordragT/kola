use std::{collections::HashMap, fmt};

use serde::{Deserialize, Serialize};

use super::{MonoType, TypeVar, Typed};
use crate::substitute::{Substitutable, Substitution};

/// Polytype
/// Types that contains variable bound by zero or more forall
/// Polymorphic types (e.g. `∀α. α → α`, `∀α. ∀β. α → β`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Polytypes
#[derive(Debug, Default, Clone, Hash, Serialize, Deserialize)]
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

    /// Two `PolyType`s are considered equivalent,
    /// if they differ only in the names of their type variables.
    pub fn alpha_equivalent(&self, other: &Self) -> bool {
        if self.vars.len() != other.vars.len() {
            return false;
        }

        // TODO maybe check for structural equality of the types

        let mut sub_self = HashMap::new();
        let mut sub_other = HashMap::new();

        for (l, r) in self.vars.iter().zip(&other.vars) {
            let fresh = MonoType::variable();
            sub_self.insert(*l, fresh.clone());
            sub_other.insert(*r, fresh);
        }

        let mut lhs = self.ty.clone();
        lhs.apply_mut(&mut Substitution::new(sub_self));

        let mut rhs = other.ty.clone();
        rhs.apply_mut(&mut Substitution::new(sub_other));

        lhs == rhs
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

/// Substitution of Polytypes in Constraint-Based Type Inference
///
/// This implementation handles substitution for polytypes in a constraint-based type inference
/// system where generalization occurs during inference and final substitution is deferred.
///
/// ## Algorithmic Context
///
/// Unlike Algorithm W (which applies substitutions eagerly), this implementation follows a
/// constraint-based approach:
/// 1. Generate constraints and generalize types during inference traversal
/// 2. Solve all constraints at the end to produce a global substitution
/// 3. Apply the final substitution to all types, including polytypes
///
/// ## The "Open Polytype" Problem
///
/// When generalization occurs during inference (e.g., at let-bindings), polytypes may contain
/// free variables that need later substitution:
///
/// ```
/// // During let-expression inference with outer scope containing T1:
/// let x = expr_of_type(T1 -> T2) in body
///
/// // Generalization with bound_vars = [T1] produces:
/// PolyType { vars: [T2], ty: T1 -> T2 }
/// //                        ^^     ^^
/// //                     free   bound
///
/// // Later constraint solving determines T1 = Int
/// // Final substitution should produce:
/// PolyType { vars: [T2], ty: Int -> T2 }
/// ```
///
/// ## Substitution Rules
///
/// When applying substitution to a polytype:
/// 1. Apply substitution to the inner monotype (`ty` field)
/// 2. Preserve all quantified variables (`vars` field unchanged)
/// 3. The inner monotype's substitution will automatically avoid bound variables
///    due to how MonoType substitution handles variable scoping
///
/// This is essential for correctness in systems where generalization and constraint
/// solving are temporally separated, which is common in modern type checkers for
/// better error reporting and incremental compilation.
impl Substitutable for PolyType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        // Apply substitution to the inner monotype while preserving quantified variables
        self.ty.try_apply(s).map(|ty| PolyType {
            vars: self.vars.clone(), // Quantified variables remain unchanged
            ty,                      // Free variables in inner type get substituted
        })
    }
}
