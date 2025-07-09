use std::{collections::HashMap, fmt};

use kola_builtins::TypeSchemeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};

use super::{MonoType, TypeVar, Typed};
use crate::{
    error::TypeConversionError,
    substitute::{Substitutable, Substitution},
    types::CompType,
};

/// Polytype
/// Types that contains variable bound by zero or more forall
/// Polymorphic types (e.g. `∀α. α → α`, `∀α. ∀β. α → β`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Polytypes
#[derive(Debug, Default, Clone, Hash, Serialize, Deserialize)]
pub struct PolyType {
    pub vars: Vec<TypeVar>,
    pub ty: MonoType,
}

impl PolyType {
    pub fn new(ty: MonoType) -> Self {
        Self {
            vars: Vec::new(),
            ty,
        }
    }

    pub fn from_protocol(scheme: TypeSchemeProtocol, interner: &mut StrInterner) -> Self {
        let TypeSchemeProtocol {
            vars_count,
            input,
            output,
        } = scheme;

        // Create exactly vars_count TypeVars
        let vars: Vec<TypeVar> = (0..vars_count).map(|_| TypeVar::new()).collect();

        // Convert types using simple array indexing
        let input = MonoType::from_protocol(input, &vars, interner);
        let output = CompType::from_protocol(output, &vars, interner);

        let ty = MonoType::func(input, output);

        PolyType { vars, ty }
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

    pub fn into_mono(self) -> Result<MonoType, TypeConversionError> {
        if self.vars.is_empty() {
            Ok(self.ty)
        } else {
            Err(TypeConversionError::NotMonomorphic(self.clone()))
        }
    }

    pub fn to_mono(&self) -> Result<MonoType, TypeConversionError> {
        if self.vars.is_empty() {
            Ok(self.ty.clone())
        } else {
            Err(TypeConversionError::NotMonomorphic(self.clone()))
        }
    }

    pub fn as_mono(&self) -> Result<&MonoType, TypeConversionError> {
        if self.vars.is_empty() {
            Ok(&self.ty)
        } else {
            Err(TypeConversionError::NotMonomorphic(self.clone()))
        }
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

        // lhs == rhs

        // Structural alpha equivalence for RowTypes is not just a simple equality check.
        todo!()
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
            write!(f, "forall")?;

            for tv in vars {
                write!(f, " {tv}")?;
            }

            write!(f, " . ")?;
        }

        ty.fmt(f)
    }
}

// TODO this comment is garbage, remove it also maybe change the implementation

/// Substitution of Polytypes in Constraint-Based Type Inference
///
/// This handles substitution for polytypes in a constraint-based system where
/// generalization occurs during inference and final substitution is deferred.
///
/// ## The Problem
///
/// When generalization occurs during inference, polytypes may contain free variables
/// that need later substitution:
///
/// ```
/// // During inference with outer scope containing T1:
/// let x = expr_of_type(T1 -> T2) in body
///
/// // Generalization produces:
/// PolyType { vars: [T2], ty: T1 -> T2 }
/// //                        ^^     ^^
/// //                     free   bound
///
/// // After constraint solving (T1 = Int):
/// PolyType { vars: [T2], ty: Int -> T2 }
/// ```
///
/// ## Substitution Strategy
///
/// 1. Apply substitution to the inner monotype
/// 2. Remove quantified variables that are resolved by the substitution
/// 3. Keep only variables that remain truly quantified
impl Substitutable for PolyType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.ty.try_apply(s).map(|ty| {
            // Remove quantified variables that have been resolved by substitution
            let vars = self
                .vars
                .iter()
                .filter(|var| !s.contains(var))
                .copied()
                .collect();

            PolyType { vars, ty }
        })
    }
}
