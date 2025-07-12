use kola_protocol::KindProtocol;
use serde::{Deserialize, Serialize};
use std::{fmt, ops::ControlFlow};

mod comp;
mod func;
mod label;
mod list;
mod module;
mod mono;
mod poly;
mod primitive;
mod row;
mod var;
mod visit;
mod wit;

pub use comp::*;
pub use func::*;
pub use label::*;
pub use list::*;
pub use module::*;
pub use mono::*;
pub use poly::*;
pub use primitive::*;
pub use row::*;
pub use var::*;
pub use visit::*;
pub use wit::*;

use super::{env::TypeClassEnv, error::TypeError};
use crate::substitute::{Substitutable, Substitution};

/// Represents a constraint on a type variable to a specific type class.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum TypeClass {
    Addable,
    Comparable,
    Equatable,
    Stringable,
}

impl Substitutable for TypeClass {
    fn try_apply(&self, _s: &mut Substitution) -> Option<Self> {
        None
    }
}

impl fmt::Display for TypeClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeClass::Addable => write!(f, "addable"),
            TypeClass::Comparable => write!(f, "comparable"),
            TypeClass::Equatable => write!(f, "equatable"),
            TypeClass::Stringable => write!(f, "stringable"),
        }
    }
}

/// Every type in the system has a kind, which is a classification of the type.
/// Used for kind preserving unification (see Extensible Records with Scoped Labels)
#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Kind {
    #[default]
    Type,
    Record,
    Label,
    Tag,
}

impl From<KindProtocol> for Kind {
    fn from(kind: KindProtocol) -> Self {
        match kind {
            KindProtocol::Type => Kind::Type,
            KindProtocol::Record => Kind::Record,
            KindProtocol::Label => Kind::Label,
            KindProtocol::Tag => Kind::Tag,
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Kind::Type => write!(f, "type"),
            Kind::Record => write!(f, "record"),
            Kind::Label => write!(f, "label"),
            Kind::Tag => write!(f, "tag"),
        }
    }
}

pub trait Typed: TypeVisitable {
    fn constrain(&self, with: TypeClass, env: &mut TypeClassEnv) -> Result<(), TypeError>;

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
