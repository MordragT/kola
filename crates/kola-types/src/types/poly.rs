use std::fmt;

use serde::{Deserialize, Serialize};

use super::{LabelOrVar, MonoType, Row, TypeConversionError, TypeVar, Typed};
use crate::{
    kind::Kind,
    substitute::{Substitutable, Substitution},
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
    pub fn instantiate(&self, mut f: impl FnMut(TypeVar, TypeVar)) -> MonoType {
        let mut ty = self.ty.clone();

        let table = self
            .forall
            .iter()
            .copied()
            .map(|from| {
                let to = from.fresh();

                f(from, to);

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

/// ## Substitution Strategy
///
/// 1. Apply substitution to the inner monotype
/// 2. Remove quantified variables that are resolved by the substitution
/// 3. Keep only variables that remain truly quantified
impl Substitutable for PolyType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.ty.try_apply(s).map(|ty| {
            // Remove quantified variables that have been resolved by substitution
            // TODO: is this really necessary ?
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
