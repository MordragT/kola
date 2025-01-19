use std::ops::ControlFlow;

pub use builtin::*;
pub use collection::*;
pub use func::*;
pub use mono::*;
pub use poly::*;
pub use record::*;
pub use var::*;
pub use visit::*;

use super::{error::SemanticError, Substitution};

mod builtin;
mod collection;
mod func;
mod mono;
mod poly;
mod record;
mod var;
mod visit;

/// Represents a constraint on a type variable to a specific kind (*i.e.*, a type class).
/// kind preserving unification (see Extensible Records with Scoped Labels)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    Addable,
    Comparable,
    Equatable,
    Stringable,
    // Logical ??
    Record,
}

pub trait Typed: TypeVisitable {
    fn constrain(&self, with: Kind, s: &mut Substitution) -> Result<(), SemanticError>;

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
