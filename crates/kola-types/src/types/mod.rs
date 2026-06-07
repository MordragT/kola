use kola_span::IntoDiagnostic;
use std::ops::ControlFlow;
use thiserror::Error;

mod comp;
mod func;
mod label;
mod list;
mod mono;
mod poly;
mod primitive;
mod record;
mod row;
mod var;
mod variant;
mod wit;

pub use comp::*;
pub use func::*;
pub use label::*;
pub use list::*;
pub use mono::*;
pub use poly::*;
pub use primitive::*;
pub use record::*;
pub use row::*;
pub use var::*;
pub use variant::*;
pub use wit::*;

use crate::{
    class::CheckClass,
    kind::CheckKind,
    visit::{TypeVisitable, TypeVisitor},
};

#[derive(Debug, Clone, Error, PartialEq, Eq)]
#[error("Cannot merge `{lhs}` and `{rhs}`")]
pub struct MergeError {
    pub lhs: MonoType,
    pub rhs: MonoType,
}

#[derive(Debug, Clone, Error)]
pub enum TypeConversionError {
    #[error("Cannot convert `{0}` to a monomorphic type")]
    NotMonomorphic(PolyType),
}

impl IntoDiagnostic for TypeConversionError {}

pub trait Typed: TypeVisitable + CheckClass + CheckKind {
    /// occurs check
    fn contains(&self, var: &TypeVar) -> bool {
        struct Occured;
        struct OccursChecker<'t>(&'t TypeVar);

        impl<'t> TypeVisitor for OccursChecker<'t> {
            type BreakValue = Occured;

            fn visit_var(&mut self, var: &TypeVar) -> ControlFlow<Self::BreakValue> {
                if var == self.0 {
                    ControlFlow::Break(Occured)
                } else {
                    ControlFlow::Continue(())
                }
            }
        }

        let mut checker = OccursChecker(var);
        match self.visit_type_by(&mut checker) {
            ControlFlow::Break(_) => true,
            ControlFlow::Continue(()) => false,
        }
    }

    fn free_vars(&self, bound: &[TypeVar]) -> Vec<TypeVar> {
        let mut vars = self.type_vars();
        vars.retain(|tv| !bound.contains(tv));
        vars.sort_unstable();
        vars.dedup();

        vars
    }

    fn type_vars(&self) -> Vec<TypeVar> {
        let mut vars = Vec::new();
        self.extend_type_vars(&mut vars);
        vars
    }

    fn extend_type_vars(&self, vars: &mut Vec<TypeVar>) {
        struct VarsFinder<'t>(&'t mut Vec<TypeVar>);

        impl<'t> TypeVisitor for VarsFinder<'t> {
            type BreakValue = !;

            fn visit_var(&mut self, var: &TypeVar) -> ControlFlow<Self::BreakValue> {
                self.0.push(*var);
                ControlFlow::Continue(())
            }
        }

        let mut finder = VarsFinder(vars);
        let ControlFlow::Continue(()) = self.visit_type_by(&mut finder);
    }
}
