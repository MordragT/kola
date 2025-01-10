use std::collections::HashMap;

use super::types::TypeVar;

/// Represents a constraint on a type variable to a specific kind (*i.e.*, a type class).
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
