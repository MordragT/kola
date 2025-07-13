use std::{collections::BTreeMap, fmt};

use kola_protocol::TypeSchemeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};

use super::{MonoType, TypeVar, Typed};
use crate::{
    constraints::Constraints,
    error::{TypeConversionError, TypeError},
    substitute::{Substitutable, Substitution},
    types::{Kind, LabelOrVar, Row},
};

/// Polytype
/// Types that contains variable bound by zero or more forall
/// Polymorphic types (e.g. `∀α. α → α`, `∀α. ∀β. α → β`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Polytypes
#[derive(Debug, Default, Clone, Hash, Serialize, Deserialize)]
pub struct PolyType {
    pub forall: Vec<TypeVar>, // forall label l row r a
    pub ty: MonoType,         // a -> { @l : a | r }
}

impl PolyType {
    pub fn new(forall: Vec<TypeVar>, ty: MonoType) -> Self {
        Self { forall, ty }
    }

    pub fn from_mono(ty: MonoType) -> Self {
        Self {
            forall: Vec::new(),
            ty,
        }
    }

    pub fn from_protocol(
        proto: TypeSchemeProtocol,
        interner: &mut StrInterner,
    ) -> Result<Self, TypeError> {
        let TypeSchemeProtocol { forall, ty } = proto;

        let mut bound = BTreeMap::new();

        // Convert types using simple array indexing
        let ty = MonoType::from_protocol(ty, &mut bound, interner)?;

        assert_eq!(forall as usize, bound.len());

        let forall = bound.into_values().collect();

        Ok(Self { forall, ty })
    }

    pub fn bound_vars(&self) -> &Vec<TypeVar> {
        &self.forall
    }

    pub fn extend_free_vars(&self, buf: &mut Vec<TypeVar>) {
        self.ty.extend_type_vars(buf);
        buf.retain(|tv| self.forall.iter().all(|var| var != tv));
    }

    pub fn free_vars(&self) -> Vec<TypeVar> {
        let mut vars = Vec::new();
        self.extend_free_vars(&mut vars);
        vars
    }

    pub fn into_mono(self) -> Result<MonoType, TypeConversionError> {
        if self.forall.is_empty() {
            Ok(self.ty)
        } else {
            Err(TypeConversionError::NotMonomorphic(self.clone()))
        }
    }

    pub fn to_mono(&self) -> Result<MonoType, TypeConversionError> {
        if self.forall.is_empty() {
            Ok(self.ty.clone())
        } else {
            Err(TypeConversionError::NotMonomorphic(self.clone()))
        }
    }

    pub fn as_mono(&self) -> Result<&MonoType, TypeConversionError> {
        if self.forall.is_empty() {
            Ok(&self.ty)
        } else {
            Err(TypeConversionError::NotMonomorphic(self.clone()))
        }
    }

    /// The procedure inst(σ) specializes the polytype σ by copying the term
    /// and replacing the bound type variables consistently by new monotype variables.
    pub fn instantiate(&self, cons: &mut Constraints) -> MonoType {
        let mut ty = self.ty.clone();

        let table = self
            .forall
            .iter()
            .copied()
            .map(|from| {
                let to = from.fresh();

                cons.constrain_inst(from, to);

                let mono_t = match from.kind() {
                    Kind::Type => MonoType::Var(to),
                    Kind::Row => MonoType::Row(Box::new(Row::Var(to))),
                    Kind::Label => MonoType::Label(LabelOrVar::Var(to)),
                };

                (from, mono_t)
            })
            .collect();
        let mut substitution = Substitution::new(table);

        ty.apply_mut(&mut substitution);

        ty
    }

    // /// Two `PolyType`s are considered equivalent,
    // /// if they differ only in the names of their type variables.
    // pub fn alpha_equivalent(&self, other: &Self) -> bool {
    //     if self.forall.len() != other.forall.len() {
    //         return false;
    //     }

    //     // TODO maybe check for structural equality of the types

    //     let mut sub_self = HashMap::new();
    //     let mut sub_other = HashMap::new();

    //     for (l, r) in self.forall.iter().zip(&other.forall) {
    //         let fresh = MonoType::variable();
    //         sub_self.insert(*l, fresh.clone());
    //         sub_other.insert(*r, fresh);
    //     }

    //     let mut lhs = self.ty.clone();
    //     lhs.apply_mut(&mut Substitution::new(sub_self));

    //     let mut rhs = other.ty.clone();
    //     rhs.apply_mut(&mut Substitution::new(sub_other));

    //     // lhs == rhs

    //     // Structural alpha equivalence for RowTypes is not just a simple equality check.
    //     todo!()
    // }
}

impl From<MonoType> for PolyType {
    fn from(ty: MonoType) -> Self {
        Self::from_mono(ty)
    }
}

impl fmt::Display for PolyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { forall, ty } = self;

        if !forall.is_empty() {
            write!(f, "forall")?;

            for kinded in forall {
                write!(f, " {kinded}")?;
            }

            write!(f, " . ")?;
        }

        ty.fmt(f)
    }
}

// TODO this comment is garbage??, remove it also maybe change the implementation
// I still just apply so that type annotations are a bit more useful

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

            let forall = self
                .forall
                .iter()
                .filter(|var| !s.contains(var))
                .copied()
                .collect();

            PolyType { forall, ty }
        })
    }
}
