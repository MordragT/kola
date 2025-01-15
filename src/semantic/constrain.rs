use std::{collections::HashMap, ops::ControlFlow};

use super::{
    error::SemanticError,
    types::{BuiltinType, FuncType, RecordType, TypeVar, TypeVisitable, TypeVisitor},
};

/// Represents a constraint on a type variable to a specific kind (*i.e.*, a type class).
/// kind preserving unification (see Extensible Records with Scoped Labels)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    Addable,
    Comparable,
    Equatable,
    Stringable,
    Record,
}

pub type Constraint = Vec<Kind>;
pub type Constraints = HashMap<TypeVar, Constraint>;

pub trait Constrainable: TypeVisitable {
    // TODO cache errors and return list of errors
    /// Validates that the current type meets the constraints of the specified kind.
    fn constrain(&self, with: Kind, constraints: &mut Constraints) -> Result<(), SemanticError> {
        let mut constrainer = Constrainer { constraints, with };
        match self.visit_type_by(&mut constrainer) {
            ControlFlow::Break(e) => Err(e),
            ControlFlow::Continue(()) => Ok(()),
        }
    }
}

struct Constrainer<'a> {
    constraints: &'a mut Constraints,
    with: Kind,
}

impl<'a> TypeVisitor for Constrainer<'a> {
    type BreakValue = SemanticError;

    fn visit_builtin(&mut self, builtin: &BuiltinType) -> ControlFlow<Self::BreakValue> {
        match builtin {
            BuiltinType::Bool => match self.with {
                Kind::Equatable | Kind::Stringable => ControlFlow::Continue(()),
                _ => ControlFlow::Break(SemanticError::CannotConstrain {
                    expected: self.with,
                    actual: builtin.into(),
                }),
            },
            BuiltinType::Num => match self.with {
                Kind::Addable | Kind::Comparable | Kind::Equatable | Kind::Stringable => {
                    ControlFlow::Continue(())
                }
                _ => ControlFlow::Break(SemanticError::CannotConstrain {
                    expected: self.with,
                    actual: builtin.into(),
                }),
            },
            BuiltinType::Char => match self.with {
                Kind::Equatable | Kind::Stringable => ControlFlow::Continue(()),
                _ => ControlFlow::Break(SemanticError::CannotConstrain {
                    expected: self.with,
                    actual: builtin.into(),
                }),
            },
            BuiltinType::Str => match self.with {
                Kind::Addable | Kind::Equatable | Kind::Stringable => ControlFlow::Continue(()),
                _ => ControlFlow::Break(SemanticError::CannotConstrain {
                    expected: self.with,
                    actual: builtin.into(),
                }),
            },
        }
    }

    fn visit_func(&mut self, func: &FuncType) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Break(SemanticError::CannotConstrain {
            expected: self.with,
            actual: func.clone().into(),
        })
    }

    fn visit_record(&mut self, record: &RecordType) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    fn visit_var(&mut self, var: &TypeVar) -> ControlFlow<Self::BreakValue> {
        self.constraints
            .entry(*var)
            .and_modify(|constraint| constraint.push(self.with))
            .or_insert_with(|| vec![self.with]);
        ControlFlow::Continue(())
    }
}
