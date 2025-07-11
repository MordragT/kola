use derive_more::{Display, From};
use kola_protocol::TypeProtocol;
use kola_utils::{
    interner::{StrInterner, StrKey},
    interner_ext::DisplayWithInterner,
};
use serde::{Deserialize, Serialize};
use std::{
    fmt,
    sync::atomic::{AtomicU32, Ordering},
};

use super::{Kind, MonoType, Typed};
use crate::{
    env::KindEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution, merge},
};

// I thought maybe label polymorphism would be useful for record merging.
// For now we'll just use a simple label type.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LabelVar(u32);

impl fmt::Display for LabelVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'l{}", self.0)
    }
}

static GENERATOR: AtomicU32 = AtomicU32::new(0);

impl LabelVar {
    pub fn new() -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
        Self(id)
    }

    pub fn id(&self) -> u32 {
        self.0
    }

    pub fn try_apply(&self, s: &mut Substitution) -> Option<Label> {
        todo!()
    }
}

#[derive(
    Debug, From, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Label {
    /// A label that is a variable.
    Var(LabelVar),
    /// A label that is a string key.
    Key(StrKey),
}

impl DisplayWithInterner<str> for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        match self {
            Self::Var(var) => write!(f, "{var}"),
            Self::Key(key) => write!(f, "{}", interner[*key]),
        }
    }
}

impl Substitutable for Label {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        match self {
            Self::Var(var) => var.try_apply(s),
            Self::Key(key) => Some(Self::Key(*key)),
        }
    }
}

/// A key-value pair representing a property type in a record.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct LabeledType {
    pub label: Label,
    pub ty: MonoType,
}

impl LabeledType {
    /// Creates a new `LabeledType` with the given label and type.
    pub fn new(label: impl Into<Label>, ty: impl Into<MonoType>) -> Self {
        Self {
            label: label.into(),
            ty: ty.into(),
        }
    }

    pub fn merge_deep(&self, other: &Self) -> Result<Self, TypeError> {
        let Label::Key(lhs) = self.label else {
            todo!("Label::Var not supported for LabeledType merging");
        };

        let Label::Key(rhs) = other.label else {
            todo!("Label::Var not supported for LabeledType merging");
        };

        if lhs != rhs {
            return Err(TypeError::CannotMergeLabel {
                label: Label::Key(lhs),
                lhs: self.ty.clone(),
                rhs: other.ty.clone(),
            });
        }

        let merged_ty = self.ty.merge_deep(&other.ty)?;
        Ok(Self {
            label: self.label,
            ty: merged_ty,
        })
    }
}

impl fmt::Display for LabeledType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.label, self.ty)
    }
}

impl Substitutable for LabeledType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        // let label = self.label.try_apply(s);
        // let ty = self.ty.try_apply(s);

        // merge(label, || self.label, ty, || self.ty.clone())
        //     .map(|(label, ty)| LabeledType { label, ty })

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

impl RowType {
    /// Creates a new `RowType::Empty`.
    #[inline]
    pub fn empty() -> Self {
        Self::Empty
    }

    /// Creates a new `RowType::Extension` with the given head and tail.
    #[inline]
    pub fn unit(head: LabeledType) -> Self {
        Self::Extension {
            head,
            tail: MonoType::Row(Box::new(RowType::Empty)),
        }
    }

    pub fn extend(self, head: LabeledType) -> Self {
        let tail = MonoType::Row(Box::new(self));

        Self::Extension { head, tail }
    }

    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        let mut fields = Vec::new();
        let mut next = self;

        while let Self::Extension {
            head: LabeledType { label, ty },
            tail,
        } = next
        {
            let Label::Key(label) = label else {
                panic!("RowType::to_protocol only supports Key labels for now");
            };

            let ty = ty.to_protocol(interner);
            fields.push((interner[*label].to_owned(), ty));

            match tail {
                MonoType::Row(row) => next = &**row,
                _ => todo!(),
            }
        }

        TypeProtocol::Record(fields)
    }

    pub fn merge_left(&self, other: &MonoType) -> Result<MonoType, TypeError> {
        match (self, other) {
            (Self::Extension { head, tail: l_tail }, r) => {
                let tail = l_tail.merge_left(r)?;

                let next = Self::Extension {
                    head: head.clone(),
                    tail,
                };
                Ok(MonoType::Row(Box::new(next)))
            }
            (Self::Empty, MonoType::Var(var)) => Ok(MonoType::Var(var.clone())),
            (Self::Empty, MonoType::Row(row)) => Ok(MonoType::Row(row.clone())),
            (lhs, rhs) => Err(TypeError::CannotMerge {
                lhs: MonoType::Row(Box::new(lhs.clone())),
                rhs: rhs.clone(),
            }),
        }
    }

    /// Below are the rules for deep row merging. In what follows monotypes
    /// and labels are denoted using lowercase letters,
    /// and type variables are not allowed.
    ///
    /// 1. r & {} or {} & r = r
    /// 2. {a: t | r} & {a: u | s} = {a: (t & u) | (r & s)}
    /// 3. {a : t | r} & {b: u | s} = {a: t | b : u | (u & r)} if a != b
    ///
    /// 1. Identity: Merging an empty row with any row results in that row.
    /// 2. Common Field: Recursively merge field types and row tails
    /// 3. Disjoint Fields: Concatenate fields; recursively merge row tails.
    ///    The 'a != b' condition merely distinguishes this case from Rule 2.
    ///    In scoped labels, distinct field order is semantically equivalent,
    ///    eliminating the need for explicit negative (absence) constraints here.
    pub fn merge_deep(&self, other: &Self) -> Result<Self, TypeError> {
        match (self, other) {
            (
                Self::Extension {
                    head: l_head,
                    tail: l_tail,
                },
                Self::Extension {
                    head: r_head,
                    tail: r_tail,
                },
            ) => {
                if l_head.label == r_head.label {
                    let head = l_head.merge_deep(&r_head)?;
                    let tail = l_tail.merge_deep(&r_tail)?;

                    Ok(Self::Extension { head, tail })
                } else {
                    let head = l_head.clone();

                    let tail = MonoType::Row(Box::new(Self::Extension {
                        head: r_head.clone(),
                        tail: l_tail.merge_deep(r_tail)?,
                    }));

                    Ok(Self::Extension { head, tail })
                }
            }

            (Self::Empty, r) => Ok(r.clone()),
            (l, Self::Empty) => Ok(l.clone()),
        }
    }
}

impl fmt::Display for RowType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "{{}}"),
            Self::Extension { head, tail } => write!(f, "{{{head} | {tail}}}"),
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
