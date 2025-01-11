use std::fmt;

use crate::{
    semantic::{
        error::InferError, merge, Cache, Constraints, Context, Kind, Substitutable,
        Substitution, Unify,
    },
    syntax::ast::Ident,
};

use super::{MonoType, TypeVar, Typed};

/// A key-value pair representing a property type in a record.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Property {
    pub k: Ident,
    pub v: MonoType,
}

impl fmt::Display for Property {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : {}", self.k, self.v)
    }
}

impl Substitutable for Property {
    fn try_apply(&self, s: &mut Substitution, cache: &mut Cache) -> Option<Self> {
        self.v.try_apply(s, cache).map(|v| Property {
            k: self.k.clone(),
            v,
        })
    }
}

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

impl Unify<&Self> for RecordType {
    // Below are the rules for record unification. In what follows monotypes
    // are denoted using lowercase letters, and type variables are denoted
    // by a lowercase letter preceded by an apostrophe `'`.
    //
    // `t = u` is read as:
    //
    //     type t unifies with type u
    //
    // `t = u => a = b` is read as:
    //
    //     if t unifies with u, then a must unify with b
    //
    // 1. Two empty records always unify, producing an empty substitution.
    // 2. {a: t | 'r} = {b: u | 'r} => error
    // 3. {a: t | 'r} = {a: u | 'r} => t = u
    // 4. {a: t |  r} = {a: u |  s} => t = u, r = s
    // 5. {a: t |  r} = {b: u |  s} => r = {b: u | 'v}, s = {a: t | 'v}
    //
    // Note rule 2. states that if two records extend the same type variable
    // they must have the same property name otherwise they cannot unify.
    //
    // self represents the expected type.
    fn unify(&self, with: &Self, ctx: &mut Context) {
        match (self, with) {
            (Self::Empty, Self::Empty) => (),
            (
                Self::Extension {
                    head: Property { k: a, v: t },
                    tail: MonoType::Var(l),
                },
                Self::Extension {
                    head: Property { k: b, v: u },
                    tail: MonoType::Var(r),
                },
            ) if a == b && l == r => {
                ctx.branch_errors(
                    |ctx| t.unify(u, ctx),
                    |cause| InferError::CannotUnifyLabel {
                        label: a.clone(),
                        expected: t.clone(),
                        actual: u.clone(),
                        cause: cause.into(),
                    },
                );
            }
            (
                Self::Extension {
                    head: Property { k: a, .. },
                    tail: MonoType::Var(l),
                },
                Self::Extension {
                    head: Property { k: b, .. },
                    tail: MonoType::Var(r),
                },
            ) if a != b && l == r => ctx.error(InferError::CannotUnify {
                expected: MonoType::from(self.clone()),
                actual: MonoType::from(with.clone()),
            }),
            (
                Self::Extension {
                    head: Property { k: a, v: t },
                    tail: l,
                },
                Self::Extension {
                    head: Property { k: b, v: u },
                    tail: r,
                },
            ) if a == b => {
                t.unify(u, ctx);
                l.unify(r, ctx);
            }
            (
                Self::Extension {
                    head: Property { k: a, v: t },
                    tail: l,
                },
                Self::Extension {
                    head: Property { k: b, v: u },
                    tail: r,
                },
            ) if a != b => {
                let var = TypeVar::new();
                let exp = MonoType::from(Self::Extension {
                    head: Property {
                        k: a.clone(),
                        v: t.clone(),
                    },
                    tail: MonoType::Var(var),
                });
                let act = MonoType::from(Self::Extension {
                    head: Property {
                        k: b.clone(),
                        v: u.clone(),
                    },
                    tail: MonoType::Var(var),
                });
                l.unify(&act, ctx);
                exp.unify(r, ctx);
            }
            // If we are expecting {a: u | r} but find {}, label `a` is missing.
            (
                Self::Extension {
                    head: Property { k: a, .. },
                    ..
                },
                Self::Empty,
            ) => ctx.error(InferError::MissingLabel(a.clone())),
            // If we are expecting {} but find {a: u | r}, label `a` is extra.
            (
                Self::Empty,
                Self::Extension {
                    head: Property { k: a, .. },
                    ..
                },
            ) => ctx.error(InferError::ExtraLabel(a.clone())),
            _ => ctx.error(InferError::CannotUnify {
                expected: MonoType::from(self.clone()),
                actual: MonoType::from(with.clone()),
            }),
        }
    }
}

impl Typed for RecordType {
    fn constrain(&self, with: Kind, constraints: &mut Constraints) -> Result<(), InferError> {
        todo!()
    }

    fn contains(&self, tv: TypeVar) -> bool {
        match self {
            Self::Empty => false,
            Self::Extension { head, tail } => head.v.contains(tv) || tail.contains(tv),
        }
    }

    fn type_vars(&self, vars: &mut Vec<TypeVar>) {
        match self {
            Self::Empty => (),
            Self::Extension { head, tail } => {
                head.v.type_vars(vars);
                tail.type_vars(vars);
            }
        }
    }
}

impl Substitutable for RecordType {
    fn try_apply(&self, s: &mut Substitution, cache: &mut Cache) -> Option<Self> {
        match self {
            Self::Empty => None,
            Self::Extension { head, tail } => {
                let h = head.try_apply(s, cache);
                let t = tail.try_apply(s, cache);

                merge(h, || head.clone(), t, || tail.clone())
                    .map(|(head, tail)| Self::Extension { head, tail })
            }
        }
    }
}
