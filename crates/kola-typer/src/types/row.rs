use kola_utils::interner::StrKey;
use serde::{Deserialize, Serialize};
use std::fmt;

use super::{Kind, MonoType, Typed};
use crate::{
    env::KindEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution, merge},
};

/// A key-value pair representing a property type in a record.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct LabeledType {
    pub label: StrKey,
    pub ty: MonoType,
}

impl fmt::Display for LabeledType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.label, self.ty)
    }
}

impl Substitutable for LabeledType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.ty.try_apply(s).map(|v| LabeledType {
            label: self.label.clone(),
            ty: v,
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

// An extensible row type.
///
/// A row is either `Empty`, meaning it has no properties,
/// or it is an extension of a row.
///
/// A record may extend what is referred to as a *row
/// variable*. A row variable is a type variable that
/// represents an unknown row type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RowType {
    /// A row that has no properties.
    Empty,
    /// Extension of a row.
    Extension {
        /// The [`Property`] that extends the row type.
        head: LabeledType,
        /// `tail` is the row variable.
        tail: MonoType,
    },
}

impl fmt::Display for RowType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "{{}}"),
            Self::Extension { head, tail } => write!(f, "{{ {head} | {tail} }}"),
        }
    }
}

impl Typed for RowType {
    fn constrain(&self, with: Kind, _env: &mut KindEnv) -> Result<(), TypeError> {
        match with {
            Kind::Record => Ok(()),
            _ => Err(TypeError::CannotConstrain {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Substitutable for RowType {
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
