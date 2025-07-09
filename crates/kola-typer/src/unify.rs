use crate::{
    error::{TypeError, TypeErrors},
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
    fn try_unify(&self, rhs: &Rhs, s: &mut Substitution) -> Result<(), TypeErrors>;
}

impl Unifiable for PrimitiveType {
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), TypeErrors> {
        let mut unifier = Unifier::new(s);
        unifier.unify_primitive(self, rhs);
        if unifier.errors.has_errors() {
            Err(unifier.errors)
        } else {
            Ok(())
        }
    }
}

impl Unifiable for FuncType {
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), TypeErrors> {
        let mut unifier = Unifier::new(s);
        unifier.unify_func(self, rhs);
        if unifier.errors.has_errors() {
            Err(unifier.errors)
        } else {
            Ok(())
        }
    }
}

impl Unifiable for CompType {
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), TypeErrors> {
        let mut unifier = Unifier::new(s);
        unifier.unify_comp(self, rhs);
        if unifier.errors.has_errors() {
            Err(unifier.errors)
        } else {
            Ok(())
        }
    }
}

impl Unifiable for MonoType {
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), TypeErrors> {
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
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), TypeErrors> {
        let mut unifier = Unifier::new(s);
        unifier.unify_row(self, rhs);
        if unifier.errors.has_errors() {
            Err(unifier.errors)
        } else {
            Ok(())
        }
    }
}

impl Unifiable for TypeVar {
    fn try_unify(&self, rhs: &Self, s: &mut Substitution) -> Result<(), TypeErrors> {
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
    fn try_unify(&self, rhs: &MonoType, s: &mut Substitution) -> Result<(), TypeErrors> {
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
    errors: TypeErrors,
}

impl<'s> Unifier<'s> {
    fn new(substitution: &'s mut Substitution) -> Self {
        Self {
            substitution,
            errors: TypeErrors::new(),
        }
    }

    fn branch_errors<F, T, E>(&mut self, f: F, context: E) -> T
    where
        F: FnOnce(&mut Self) -> T,
        E: FnOnce(TypeErrors) -> TypeError,
    {
        let former = self.errors.take();
        let result = f(self);
        let errors = self.errors.replace(former);
        self.errors.push(context(errors));

        result
    }

    fn unify_primitive(&mut self, lhs: &PrimitiveType, rhs: &PrimitiveType) {
        if lhs != rhs {
            self.errors.push(TypeError::CannotUnify {
                expected: lhs.into(),
                actual: rhs.into(),
            });
        }
    }

    fn unify_func(&mut self, lhs: &FuncType, rhs: &FuncType) {
        self.unify_mono(&lhs.input, &rhs.input);
        self.unify_comp(&lhs.output, &rhs.output);
    }

    fn unify_list(&mut self, lhs: &ListType, rhs: &ListType) {
        self.unify_mono(&lhs.el, &rhs.el);
    }

    fn unify_mono(&mut self, lhs: &MonoType, rhs: &MonoType) {
        // Apply current substitution to resolve any already-bound variables
        let lhs = lhs.apply_cow(self.substitution);
        let rhs = rhs.apply_cow(self.substitution);

        match (lhs.as_ref(), rhs.as_ref()) {
            (MonoType::Primitive(l), MonoType::Primitive(r)) => self.unify_primitive(l, r),
            (MonoType::Func(l), MonoType::Func(r)) => self.unify_func(l, r),
            (MonoType::List(l), MonoType::List(r)) => self.unify_list(l, r),
            (MonoType::Row(l), MonoType::Row(r)) => self.unify_row(l, r),
            (MonoType::Var(var), with) => self.bind_var(var, with),
            (with, MonoType::Var(var)) => self.bind_var(var, with),
            (l, r) => {
                self.errors.push(TypeError::CannotUnify {
                    expected: l.clone(),
                    actual: r.clone(),
                });
            }
        }
    }

    fn unify_comp(&mut self, lhs: &CompType, rhs: &CompType) {
        // Apply current substitution to resolve any already-bound variables
        let lhs = lhs.apply_cow(self.substitution);
        let rhs = rhs.apply_cow(self.substitution);

        self.unify_mono(&lhs.ty, &rhs.ty);
        self.unify_row(&lhs.effect, &rhs.effect);
    }

    // Below are the rules for row unification. In what follows monotypes
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
    // 1. Two empty rows always unify, producing an empty substitution.
    // 2. {a: t | 'r} = {b: u | 'r} => error
    // 3. {a: t | 'r} = {a: u | 'r} => t = u
    // 4. {a: t |  r} = {a: u |  s} => t = u, r = s
    // 5. {a: t |  r} = {b: u |  s} => r = {b: u | 'v}, s = {a: t | 'v}
    //
    // Note rule 2. states that if two rows extend the same type variable
    // they must have the same property name otherwise they cannot unify.
    //
    // self represents the expected type.
    fn unify_row(&mut self, lhs: &RowType, rhs: &RowType) {
        match (lhs, rhs) {
            (RowType::Empty, RowType::Empty) => (),
            (
                RowType::Extension {
                    head: LabeledType { label: a, ty: t },
                    tail: MonoType::Var(l),
                },
                RowType::Extension {
                    head: LabeledType { label: b, ty: u },
                    tail: MonoType::Var(r),
                },
            ) if a == b && l == r => {
                self.branch_errors(
                    |ctx| ctx.unify_mono(t, u),
                    |cause| TypeError::CannotUnifyLabel {
                        label: a.clone(),
                        expected: t.clone(),
                        actual: u.clone(),
                        cause: cause.into(),
                    },
                );
            }
            (
                RowType::Extension {
                    head: LabeledType { label: a, .. },
                    tail: MonoType::Var(l),
                },
                RowType::Extension {
                    head: LabeledType { label: b, .. },
                    tail: MonoType::Var(r),
                },
            ) if a != b && l == r => self.errors.push(TypeError::CannotUnify {
                expected: MonoType::from(lhs.clone()),
                actual: MonoType::from(rhs.clone()),
            }),
            (
                RowType::Extension {
                    head: LabeledType { label: a, ty: t },
                    tail: l,
                },
                RowType::Extension {
                    head: LabeledType { label: b, ty: u },
                    tail: r,
                },
            ) if a == b => {
                self.unify_mono(t, u);
                self.unify_mono(l, r);
            }
            (
                RowType::Extension {
                    head: LabeledType { label: a, ty: t },
                    tail: l,
                },
                RowType::Extension {
                    head: LabeledType { label: b, ty: u },
                    tail: r,
                },
            ) if a != b => {
                let var = TypeVar::new();
                let exp = MonoType::from(RowType::Extension {
                    head: LabeledType {
                        label: a.clone(),
                        ty: t.clone(),
                    },
                    tail: MonoType::Var(var),
                });
                let act = MonoType::from(RowType::Extension {
                    head: LabeledType {
                        label: b.clone(),
                        ty: u.clone(),
                    },
                    tail: MonoType::Var(var),
                });
                self.unify_mono(l, &act);
                self.unify_mono(&exp, r);
            }
            // If we are expecting {a: u | r} but find {}, label `a` is missing.
            (
                RowType::Extension {
                    head: LabeledType { label: a, .. },
                    ..
                },
                RowType::Empty,
            ) => self.errors.push(TypeError::MissingLabel(a.clone())),
            // If we are expecting {} but find {a: u | r}, label `a` is extra.
            (
                RowType::Empty,
                RowType::Extension {
                    head: LabeledType { label: a, .. },
                    ..
                },
            ) => self.errors.push(TypeError::ExtraLabel(a.clone())),
            _ => self.errors.push(TypeError::CannotUnify {
                expected: MonoType::from(lhs.clone()),
                actual: MonoType::from(rhs.clone()),
            }),
        }
    }

    fn unify_var(&mut self, lhs: &TypeVar, rhs: &TypeVar) {
        if lhs != rhs {
            // in former apply path compression via cache is already implemented
            // so this should not become essentially an inefficient linked list
            self.substitution.insert(*lhs, MonoType::Var(*rhs));
        }
    }

    // TODO order here really correct ?
    fn bind_var(&mut self, var: &TypeVar, with: &MonoType) {
        if let MonoType::Var(with) = with {
            self.unify_var(var, with);
        } else if with.contains(var) {
            self.errors.push(TypeError::Occurs(*var));
        } else {
            self.substitution.insert(*var, with.clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use kola_utils::interner::StrInterner;

    #[test]
    fn test_same_row_variable_different_extensions_should_fail() {
        let mut interner = StrInterner::new();
        let mut subs = Substitution::empty();
        let shared_var = TypeVar::new();

        // Type 1: { name : Str | shared_var }
        let type1 = MonoType::row(
            LabeledType::new(interner.intern("name"), MonoType::STR),
            MonoType::Var(shared_var),
        );

        // Type 2: { name : Str, age : Num }
        let type2 = MonoType::row(
            LabeledType::new(interner.intern("name"), MonoType::STR),
            MonoType::row(
                LabeledType::new(interner.intern("age"), MonoType::NUM),
                MonoType::empty_row(),
            ),
        );

        // This should succeed and bind shared_var = { age : Num | {} }
        let result1 = type1.try_unify(&type2, &mut subs);
        assert!(result1.is_ok());

        // Type 3: { name : Str, car : Str }
        let type3 = MonoType::row(
            LabeledType::new(interner.intern("name"), MonoType::STR),
            MonoType::row(
                LabeledType::new(interner.intern("car"), MonoType::STR),
                MonoType::empty_row(),
            ),
        );

        // Type 4: { name : Str | shared_var } (same shared_var!)
        let type4 = MonoType::row(
            LabeledType::new(interner.intern("name"), MonoType::STR),
            MonoType::Var(shared_var),
        );

        // This should FAIL because shared_var is already bound
        let result2 = type4.try_unify(&type3, &mut subs);
        assert!(
            result2.is_err(),
            "Should fail because shared_var cannot be both {{age:Num}} and {{car:Str}}"
        );
    }

    #[test]
    fn test_record_unification_with_annotation() {
        let mut interner = StrInterner::new();
        let mut subs = Substitution::empty();
        let shared_row_var = TypeVar::new();

        // Person a = { name : Str | a }
        let person_a = MonoType::row(
            LabeledType::new(interner.intern("name"), MonoType::STR),
            MonoType::Var(shared_row_var),
        );

        // Annotation type: { zero : Person a, one : Person a }
        let annotation = MonoType::row(
            LabeledType::new(interner.intern("zero"), person_a.clone()),
            MonoType::row(
                LabeledType::new(interner.intern("one"), person_a.clone()),
                MonoType::empty_row(),
            ),
        );

        // alice_type = { name : Str, age : Num }
        let alice_type = MonoType::row(
            LabeledType::new(interner.intern("name"), MonoType::STR),
            MonoType::row(
                LabeledType::new(interner.intern("age"), MonoType::NUM),
                MonoType::empty_row(),
            ),
        );

        // bob_type = { name : Str, car : Str }
        let bob_type = MonoType::row(
            LabeledType::new(interner.intern("name"), MonoType::STR),
            MonoType::row(
                LabeledType::new(interner.intern("car"), MonoType::STR),
                MonoType::empty_row(),
            ),
        );

        let inferred = MonoType::row(
            LabeledType::new(interner.intern("zero"), alice_type),
            MonoType::row(
                LabeledType::new(interner.intern("one"), bob_type),
                MonoType::empty_row(),
            ),
        );

        // This should FAIL because the same row variable cannot have different extensions
        let result = annotation.try_unify(&inferred, &mut subs);
        assert!(
            result.is_err(),
            "Should fail because the same row variable cannot have different extensions"
        );
    }

    #[test]
    fn test_occurs_check() {
        let mut interner = StrInterner::new();
        let mut subs = Substitution::empty();
        let var = TypeVar::new();

        // Create a recursive type: var = { field : var }
        let recursive_type = MonoType::row(
            LabeledType::new(interner.intern("field"), MonoType::Var(var)),
            MonoType::empty_row(),
        );

        let result = MonoType::Var(var).try_unify(&recursive_type, &mut subs);
        assert!(result.is_err(), "Should fail due to occurs check");
    }

    #[test]
    fn test_function_unification() {
        let mut subs = Substitution::empty();

        // f1: Int -> String
        let f1 = MonoType::pure_func(MonoType::NUM, MonoType::STR);

        // f2: Int -> Bool (should fail)
        let f2 = MonoType::pure_func(MonoType::NUM, MonoType::BOOL);

        let result = f1.try_unify(&f2, &mut subs);
        assert!(result.is_err(), "Should fail - different return types");
    }

    #[test]
    fn test_function_with_variables() {
        let mut subs = Substitution::empty();
        let var_a = TypeVar::new();
        let var_b = TypeVar::new();

        // f1: a -> a (identity function)
        let f1 = MonoType::pure_func(MonoType::Var(var_a), MonoType::Var(var_a));

        // f2: Int -> b
        let f2 = MonoType::pure_func(MonoType::NUM, MonoType::Var(var_b));

        let result = f1.try_unify(&f2, &mut subs);
        assert!(result.is_ok(), "Should unify with a=Int, b=Int");

        // Check that both variables are bound to Int
        let expected_int = MonoType::NUM;
        assert_eq!(subs.get(&var_a), Some(&expected_int));
        assert_eq!(subs.get(&var_b), Some(&expected_int));
    }

    #[test]
    fn test_complex_row_reordering() {
        let mut interner = StrInterner::new();
        let mut subs = Substitution::empty();

        // Type 1: { a : Int, b : String }
        let type1 = MonoType::row(
            LabeledType::new(interner.intern("a"), MonoType::NUM),
            MonoType::row(
                LabeledType::new(interner.intern("b"), MonoType::STR),
                MonoType::empty_row(),
            ),
        );

        // Type 2: { b : String, a : Int } (different order)
        let type2 = MonoType::row(
            LabeledType::new(interner.intern("b"), MonoType::STR),
            MonoType::row(
                LabeledType::new(interner.intern("a"), MonoType::NUM),
                MonoType::empty_row(),
            ),
        );

        let result = type1.try_unify(&type2, &mut subs);
        assert!(result.is_ok(), "Should unify despite different field order");
    }

    #[test]
    fn test_missing_and_extra_labels() {
        let mut interner = StrInterner::new();
        let mut subs = Substitution::empty();

        // Type 1: { a : Int }
        let type1 = MonoType::row(
            LabeledType::new(interner.intern("a"), MonoType::NUM),
            MonoType::empty_row(),
        );

        // Type 2: {} (empty record)
        let type2 = MonoType::empty_row();

        let result = type1.try_unify(&type2, &mut subs);
        assert!(result.is_err(), "Should fail - missing label 'a'");

        // Reverse: extra label
        let result = type2.try_unify(&type1, &mut subs);
        assert!(result.is_err(), "Should fail - extra label 'a'");
    }

    #[test]
    fn test_row_variable_chaining() {
        let mut interner = StrInterner::new();
        let mut subs = Substitution::empty();
        let var1 = TypeVar::new();
        let var2 = TypeVar::new();

        // First bind var1 = var2
        let result1 = MonoType::Var(var1).try_unify(&MonoType::Var(var2), &mut subs);
        assert!(result1.is_ok());

        // Then bind var2 = { field : Int }
        let concrete_type = MonoType::row(
            LabeledType::new(interner.intern("field"), MonoType::NUM),
            MonoType::empty_row(),
        );

        let result2 = MonoType::Var(var2).try_unify(&concrete_type, &mut subs);
        assert!(result2.is_ok());

        // Now var1 should resolve to the concrete type through var2
        let resolved_var1 = MonoType::Var(var1).apply(&mut subs);
        assert_eq!(resolved_var1, concrete_type);
    }

    #[test]
    fn test_incompatible_types() {
        let mut interner = StrInterner::new();
        let mut subs = Substitution::empty();

        let int_type = MonoType::NUM;
        let str_type = MonoType::STR;
        let record_type = MonoType::row(
            LabeledType::new(interner.intern("field"), MonoType::NUM),
            MonoType::empty_row(),
        );
        let func_type = MonoType::pure_func(int_type.clone(), str_type.clone());

        // Test all incompatible pairs
        assert!(int_type.try_unify(&str_type, &mut subs).is_err());
        assert!(int_type.try_unify(&record_type, &mut subs).is_err());
        assert!(int_type.try_unify(&func_type, &mut subs).is_err());
        assert!(record_type.try_unify(&func_type, &mut subs).is_err());
    }
}
