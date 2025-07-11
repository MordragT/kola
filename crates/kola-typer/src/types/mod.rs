use derive_more::Display;
use serde::{Deserialize, Serialize};
use std::{fmt, ops::ControlFlow};

mod comp;
mod func;
mod list;
mod module;
mod mono;
mod poly;
mod primitive;
mod row;
mod type_rep;
mod var;
mod visit;

pub use comp::*;
pub use func::*;
pub use list::*;
pub use module::*;
pub use mono::*;
pub use poly::*;
pub use primitive::*;
pub use row::*;
pub use type_rep::*;
pub use var::*;
pub use visit::*;

use super::{env::KindEnv, error::TypeError};
use crate::substitute::{Substitutable, Substitution};

// pub type Kinded<T> = (T, Kind);

/// Represents a constraint on a type variable to a specific kind (*i.e.*, a type class).
/// kind preserving unification (see Extensible Records with Scoped Labels)
#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Kind {
    #[default]
    Type,
    Addable,
    Comparable,
    Equatable,
    Stringable,
    Record,
    Label,
}

impl Substitutable for Kind {
    fn try_apply(&self, _s: &mut Substitution) -> Option<Self> {
        None
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Kind::Type => write!(f, "type"),
            Kind::Addable => write!(f, "addable"),
            Kind::Comparable => write!(f, "comparable"),
            Kind::Equatable => write!(f, "equatable"),
            Kind::Stringable => write!(f, "stringable"),
            Kind::Record => write!(f, "record"),
            Kind::Label => write!(f, "label"),
        }
    }
}

pub trait Typed: TypeVisitable {
    fn constrain(&self, with: Kind, env: &mut KindEnv) -> Result<(), TypeError>;

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
