use std::fmt;

use crate::{
    semantic::{merge, Substitutable, Substitution},
    syntax::ast::Symbol,
};

use super::{MonoType, Typed};

/// A key-value pair representing a property type in a record.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Property {
    pub k: Symbol,
    pub v: MonoType,
}

impl fmt::Display for Property {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.k, self.v)
    }
}

impl Substitutable for Property {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.v.try_apply(s).map(|v| Property {
            k: self.k.clone(),
            v,
        })
    }
}

/*
Rule (eq-head) defines two rows as equal
when their heads and tails are equal. The rule (eq-swap)is the most
interesting: it states that the first two fields of a row can be swapped
if (and only if) their labels are different. Together with transitivity
(eq-trans) and row equality (eq-head), this effectively allows us
to swap a field repeatedly to the front of a record, but not past an
equal label. With the new notion of equality, we can immediately
derive that:
{x :: Int, y :: Int} ∼= {y :: Int, x :: Int}
*/

// An extensible record type.
///
/// A record is either `Empty`, meaning it has no properties,
/// or it is an extension of a record.
///
/// A record may extend what is referred to as a *record
/// variable*. A record variable is a type variable that
/// represents an unknown record type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordType {
    /// A record that has no properties.
    Empty,
    /// Extension of a record.
    Extension {
        /// The [`Property`] that extends the record type.
        head: Property,
        /// `tail` is the record variable.
        tail: MonoType,
    },
}

impl fmt::Display for RecordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "{{}}"),
            Self::Extension { head, tail } => write!(f, "{{ {head} | {tail} }}"),
        }
    }
}

impl Typed for RecordType {}

impl Substitutable for RecordType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        match self {
            Self::Empty => None,
            Self::Extension { head, tail } => {
                let h = head.try_apply(s);
                let t = tail.try_apply(s);

                merge(h, || head.clone(), t, || tail.clone())
                    .map(|(head, tail)| Self::Extension { head, tail })
            }
        }
    }
}
