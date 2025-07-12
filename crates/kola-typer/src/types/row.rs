use derive_more::{Display, From};
use kola_protocol::TypeProtocol;
use kola_utils::{interner::StrInterner, interner_ext::DisplayWithInterner};
use serde::{Deserialize, Serialize};
use std::fmt;

use super::{Kind, Label, MonoType, TypeClass, TypeVar, Typed};
use crate::{
    env::TypeClassEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution, merge},
};

#[derive(
    Debug, Display, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum LabelOrVar {
    /// A label that is a variable.
    Var(TypeVar),
    /// A label that is a string key.
    Label(Label),
}

impl LabelOrVar {
    /// Creates a new `LabelOrVar` variable with a new type variable.
    #[inline]
    pub fn var() -> Self {
        Self::Var(TypeVar::new(Kind::Label))
    }

    pub fn can_unify(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Var(var), _) | (_, Self::Var(var)) => {
                debug_assert_eq!(var.kind(), Kind::Label);
                true
            }
            (Self::Label(lhs), Self::Label(rhs)) => lhs == rhs,
        }
    }
}

impl DisplayWithInterner<str> for LabelOrVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        match self {
            Self::Var(var) => write!(f, "{var}"),
            Self::Label(key) => key.fmt(f, interner),
        }
    }
}

impl Substitutable for LabelOrVar {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        match self {
            Self::Var(var) => var.try_apply(s).map(|mono| {
                let label = mono
                    .into_label()
                    .expect("MonoType should be convertible to Label");
                Self::Label(label)
            }),
            Self::Label(key) => Some(Self::Label(*key)),
        }
    }
}

/// A key-value pair representing a property type in a record.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct LabeledType {
    pub label: LabelOrVar,
    pub ty: MonoType,
}

impl LabeledType {
    /// Creates a new `LabeledType` with the given label and type.
    pub fn new(label: impl Into<LabelOrVar>, ty: impl Into<MonoType>) -> Self {
        Self {
            label: label.into(),
            ty: ty.into(),
        }
    }

    pub fn merge_deep(&self, other: &Self) -> Result<Self, TypeError> {
        if let (LabelOrVar::Var(_), LabelOrVar::Var(_)) = (&self.label, &other.label) {
            todo!("Merging of label variables is not implemented yet");
        }

        if let (MonoType::Row(l), MonoType::Row(r)) = (&self.ty, &other.ty) {
            let merged = l.merge_deep(r)?;

            Ok(Self {
                label: self.label.clone(),
                ty: MonoType::Row(Box::new(merged)),
            })
        } else {
            Err(TypeError::CannotMerge {
                lhs: self.ty.clone(),
                rhs: other.ty.clone(),
            })
        }
    }
}

impl fmt::Display for LabeledType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.label, self.ty)
    }
}

impl Substitutable for LabeledType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let label = self.label.try_apply(s);
        let ty = self.ty.try_apply(s);

        merge(label, || self.label, ty, || self.ty.clone())
            .map(|(label, ty)| LabeledType { label, ty })
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

// TODO RowType's tail shouldn't be a `MonoType` but rather a `RowType`.
// In order to do that, we need to change the RowType to include a
// type variable (of kind row)

// An extensible row type.
///
/// A row is either `Empty`, meaning it has no properties,
/// or it is an extension of a row.
///
/// A record may extend what is referred to as a *row
/// variable*. A row variable is a type variable that
/// represents an unknown row type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Row {
    /// A row that has no properties.
    Empty,
    /// A row variable for polymorphic rows.
    Var(TypeVar),
    /// Extension of a row.
    Extension {
        /// The [`Property`] that extends the row type.
        head: LabeledType,
        /// `tail` is the row variable.
        tail: Box<Self>,
    },
}

impl Row {
    /// Creates a new `RowType::Empty`.
    #[inline]
    pub fn empty() -> Self {
        Self::Empty
    }

    /// Creates a new `RowType::Var` with a new row variable.
    #[inline]
    pub fn var() -> Self {
        Self::Var(TypeVar::new(Kind::Row))
    }

    /// Creates a new `RowType::Extension` with the given head and tail.
    #[inline]
    pub fn unit(head: LabeledType) -> Self {
        Self::Extension {
            head,
            tail: Box::new(Row::Empty),
        }
    }

    pub fn extend(self, head: LabeledType) -> Self {
        let tail = Box::new(self);

        Self::Extension { head, tail }
    }

    pub fn to_protocol(&self, interner: &StrInterner) -> Vec<(String, TypeProtocol)> {
        let mut rows = Vec::new();
        let mut next = self;

        while let Self::Extension {
            head: LabeledType { label, ty },
            tail,
        } = next
        {
            let LabelOrVar::Label(label) = label else {
                todo!("RowType::to_protocol only supports Key labels for now");
            };

            let ty = ty.to_protocol(interner);
            rows.push((interner[label.0].to_owned(), ty));

            next = tail;
        }

        rows
    }

    /// Only works, if the left-hand side is a concrete row type.
    /// The right-hand side can be a row type or a row variable.
    pub fn merge_left(&self, other: &Self) -> Result<Self, TypeError> {
        match (self, other) {
            (Self::Extension { head, tail: l_tail }, r) => {
                let tail = Box::new(l_tail.merge_left(r)?);

                let next = Self::Extension {
                    head: head.clone(),
                    tail,
                };
                Ok(next)
            }
            (Self::Empty, r) => Ok(r.clone()),
            (lhs, rhs) => Err(TypeError::CannotMerge {
                lhs: MonoType::Row(Box::new(lhs.clone())),
                rhs: MonoType::Row(Box::new(rhs.clone())),
            }),
        }
    }

    /// Only works, if the right-hand side is a concrete row type.
    /// The left-hand side can be a row type or a row variable.
    #[inline]
    pub fn merge_right(&self, other: &Self) -> Result<Self, TypeError> {
        other.merge_left(self)
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
                if l_head.label.can_unify(&r_head.label) {
                    let head = l_head.merge_deep(&r_head)?;
                    let tail = Box::new(l_tail.merge_deep(&r_tail)?);

                    Ok(Self::Extension { head, tail })
                } else {
                    let head = l_head.clone();

                    let tail = Box::new(Self::Extension {
                        head: r_head.clone(),
                        tail: Box::new(l_tail.merge_deep(r_tail)?),
                    });

                    Ok(Self::Extension { head, tail })
                }
            }

            (Self::Empty, r) => Ok(r.clone()),
            (l, Self::Empty) => Ok(l.clone()),
            _ => Err(TypeError::CannotMerge {
                lhs: MonoType::Row(Box::new(self.clone())),
                rhs: MonoType::Row(Box::new(other.clone())),
            }),
        }
    }
}

impl fmt::Display for Row {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "{{}}"),
            Self::Var(var) => write!(f, "{var}"),
            Self::Extension { head, tail } => write!(f, "{{{head} | {tail}}}"),
        }
    }
}

impl Typed for Row {
    fn kind(&self) -> Kind {
        Kind::Row
    }

    fn constrain(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeError> {
        match with {
            _ => Err(TypeError::CannotConstrain {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Substitutable for Row {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        match self {
            Self::Empty => None,
            Self::Var(var) => {
                debug_assert!(
                    var.kind() == Kind::Row,
                    "RowType::Var should be a row variable"
                );
                var.try_apply(s).map(|mono| *mono.into_row().unwrap())
            }
            Self::Extension { head, tail } => {
                let h = head.try_apply(s);
                let t = tail.try_apply(s);

                merge(h, || head.clone(), t, || *tail.clone()).map(|(head, tail)| Self::Extension {
                    head,
                    tail: Box::new(tail),
                })
            }
        }
    }
}
