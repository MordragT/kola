use kola_utils::errors::Errors;

use crate::{
    error::SemanticError,
    substitute::{Substitutable, Substitution},
    types::*,
};

/// Principal Type: Most general type that can be inferred for a given expression
/// Unify algorithm in J performs mutation, in W it does not.
/// Builds up constraints (Substitutions) within the context so that lhs and rhs unify.
/// Most general unifier, builds up a substitution S such that S(lhs) is congruent to S(rhs).
pub trait Unifiable<Rhs = Self> {
    /// Performs unification on the type with another type.
    /// If successful, results in a solution to the unification problem,
    /// in the form of a substitution. If there is no solution to the
    /// unification problem then unification fails and an error is reported.
    fn try_unify(&self, rhs: &Rhs, s: &mut Substitution) -> Result<(), Errors<SemanticError>>;
}

impl Unifiable for BuiltinType {
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), Errors<SemanticError>> {
        let mut unifier = Unifier::new(s);
        unifier.unify_builtin(self, rhs);
        if unifier.errors.has_errors() {
            Err(unifier.errors)
        } else {
            Ok(())
        }
    }
}

impl Unifiable for FuncType {
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), Errors<SemanticError>> {
        let mut unifier = Unifier::new(s);
        unifier.unify_func(self, rhs);
        if unifier.errors.has_errors() {
            Err(unifier.errors)
        } else {
            Ok(())
        }
    }
}

impl Unifiable for MonoType {
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), Errors<SemanticError>> {
        let mut unifier = Unifier::new(s);
        unifier.unify_mono(self, rhs);
        if unifier.errors.has_errors() {
            Err(unifier.errors)
        } else {
            Ok(())
        }
    }
}

impl Unifiable for RowType {
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), Errors<SemanticError>> {
        let mut unifier = Unifier::new(s);
        unifier.unify_record(self, rhs);
        if unifier.errors.has_errors() {
            Err(unifier.errors)
        } else {
            Ok(())
        }
    }
}

impl Unifiable for TypeVar {
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), Errors<SemanticError>> {
        let mut unifier = Unifier::new(s);
        unifier.unify_var(self, rhs);
        if unifier.errors.has_errors() {
            Err(unifier.errors)
        } else {
            Ok(())
        }
    }
}

impl Unifiable<MonoType> for TypeVar {
    fn try_unify(&self, rhs: &MonoType, s: &mut Substitution) -> Result<(), Errors<SemanticError>> {
        let mut unifier = Unifier::new(s);
        unifier.bind_var(self, rhs);
        if unifier.errors.has_errors() {
            Err(unifier.errors)
        } else {
            Ok(())
        }
    }
}

struct Unifier<'s> {
    substitution: &'s mut Substitution,
    errors: Errors<SemanticError>,
}

impl<'s> Unifier<'s> {
    fn new(substitution: &'s mut Substitution) -> Self {
        Self {
            substitution,
            errors: Errors::new(),
        }
    }

    fn branch_errors<F, T, E>(&mut self, f: F, context: E) -> T
    where
        F: FnOnce(&mut Self) -> T,
        E: FnOnce(Errors<SemanticError>) -> SemanticError,
    {
        let former = self.errors.take();
        let result = f(self);
        let errors = self.errors.replace(former);
        self.errors.push(context(errors));

        result
    }

    fn unify_builtin(&mut self, lhs: &BuiltinType, rhs: &BuiltinType) {
        if lhs != rhs {
            self.errors.push(SemanticError::CannotUnify {
                expected: lhs.into(),
                actual: rhs.into(),
            });
        }
    }

    fn unify_func(&mut self, lhs: &FuncType, rhs: &FuncType) {
        self.unify_mono(&lhs.input, &rhs.input);
        let l = lhs.output.apply_cow(self.substitution);
        let r = rhs.output.apply_cow(self.substitution);
        self.unify_mono(&l, &r);
    }

    fn unify_mono(&mut self, lhs: &MonoType, rhs: &MonoType) {
        match (lhs, rhs) {
            (MonoType::Builtin(l), MonoType::Builtin(r)) => self.unify_builtin(l, r),
            (MonoType::Func(l), MonoType::Func(r)) => self.unify_func(l, r),
            (MonoType::Row(l), MonoType::Row(r)) => self.unify_record(l, r),
            (MonoType::Var(var), with) => self.bind_var(var, with),
            (with, MonoType::Var(var)) => self.bind_var(var, with),
            (l, r) => {
                self.errors.push(SemanticError::CannotUnify {
                    expected: l.clone(),
                    actual: r.clone(),
                });
            }
        }
    }

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
    fn unify_record(&mut self, lhs: &RowType, rhs: &RowType) {
        match (lhs, rhs) {
            (RowType::Empty, RowType::Empty) => (),
            (
                RowType::Extension {
                    head: Property { k: a, v: t },
                    tail: MonoType::Var(l),
                },
                RowType::Extension {
                    head: Property { k: b, v: u },
                    tail: MonoType::Var(r),
                },
            ) if a == b && l == r => {
                self.branch_errors(
                    |ctx| ctx.unify_mono(t, u),
                    |cause| SemanticError::CannotUnifyLabel {
                        label: a.clone(),
                        expected: t.clone(),
                        actual: u.clone(),
                        cause: cause.into(),
                    },
                );
            }
            (
                RowType::Extension {
                    head: Property { k: a, .. },
                    tail: MonoType::Var(l),
                },
                RowType::Extension {
                    head: Property { k: b, .. },
                    tail: MonoType::Var(r),
                },
            ) if a != b && l == r => self.errors.push(SemanticError::CannotUnify {
                expected: MonoType::from(lhs.clone()),
                actual: MonoType::from(rhs.clone()),
            }),
            (
                RowType::Extension {
                    head: Property { k: a, v: t },
                    tail: l,
                },
                RowType::Extension {
                    head: Property { k: b, v: u },
                    tail: r,
                },
            ) if a == b => {
                self.unify_mono(t, u);
                self.unify_mono(l, r);
            }
            (
                RowType::Extension {
                    head: Property { k: a, v: t },
                    tail: l,
                },
                RowType::Extension {
                    head: Property { k: b, v: u },
                    tail: r,
                },
            ) if a != b => {
                let var = TypeVar::new();
                let exp = MonoType::from(RowType::Extension {
                    head: Property {
                        k: a.clone(),
                        v: t.clone(),
                    },
                    tail: MonoType::Var(var),
                });
                let act = MonoType::from(RowType::Extension {
                    head: Property {
                        k: b.clone(),
                        v: u.clone(),
                    },
                    tail: MonoType::Var(var),
                });
                self.unify_mono(l, &act);
                self.unify_mono(&exp, r);
            }
            // If we are expecting {a: u | r} but find {}, label `a` is missing.
            (
                RowType::Extension {
                    head: Property { k: a, .. },
                    ..
                },
                RowType::Empty,
            ) => self.errors.push(SemanticError::MissingLabel(a.clone())),
            // If we are expecting {} but find {a: u | r}, label `a` is extra.
            (
                RowType::Empty,
                RowType::Extension {
                    head: Property { k: a, .. },
                    ..
                },
            ) => self.errors.push(SemanticError::ExtraLabel(a.clone())),
            _ => self.errors.push(SemanticError::CannotUnify {
                expected: MonoType::from(lhs.clone()),
                actual: MonoType::from(rhs.clone()),
            }),
        }
    }

    fn unify_var(&mut self, lhs: &TypeVar, rhs: &TypeVar) {
        if lhs != rhs {
            // ctx.error(InferError::CannotUnify {
            //     expected: self.into(),
            //     actual: with.into(),
            // })

            // in former apply path compression via cache is already implemented
            // so this should not become essentially an inefficient linked list
            self.substitution.insert(*lhs, MonoType::Var(*rhs));
        }
    }

    fn bind_var(&mut self, var: &TypeVar, with: &MonoType) {
        if let MonoType::Var(with) = with {
            self.unify_var(var, with);
        } else if with.contains(var) {
            self.errors.push(SemanticError::Occurs(*var));
        } else {
            self.substitution.insert(*var, with.clone());
        }
    }
}

/// Helper function that concatenates two vectors into a single vector while removing duplicates.
pub(crate) fn union<T: PartialEq>(mut vars: Vec<T>, mut with: Vec<T>) -> Vec<T> {
    with.retain(|tv| !vars.contains(tv));
    vars.append(&mut with);
    vars
}
