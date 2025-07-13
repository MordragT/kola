use derive_more::Display;
use kola_span::{Loc, Located};
use kola_utils::errors::Errors;
use log::trace;

use crate::{
    env::TypeClassEnv,
    error::{TypeError, TypeErrors},
    substitute::{Substitutable, Substitution},
    types::{MonoType, TypeClass, TypeVar, Typed},
    unify::Unifiable,
};

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MergeKind {
    Left,
    Right,
    Deep,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint {
    Inst {
        from: TypeVar, // the variable from the scheme
        to: TypeVar,   // the fresh variable created
    },
    Class {
        expected: TypeClass,
        actual: MonoType,
        span: Loc,
    },
    Equal {
        expected: MonoType,
        actual: MonoType,
        span: Loc,
    },
    Merge {
        result: TypeVar,
        lhs: MonoType,
        rhs: MonoType,
        kind: MergeKind,
        span: Loc,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraints(Vec<Constraint>);

impl Constraints {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn constrain_inst(&mut self, from: TypeVar, to: TypeVar) {
        let c = Constraint::Inst { from, to };
        self.0.push(c);
    }

    pub fn constrain_class(&mut self, expected: TypeClass, actual: MonoType, span: Loc) {
        let c = Constraint::Class {
            expected,
            actual,
            span,
        };
        self.0.push(c);
    }

    pub fn constrain_equal(&mut self, expected: MonoType, actual: MonoType, span: Loc) {
        let c = Constraint::Equal {
            expected,
            actual,
            span,
        };
        self.0.push(c);
    }

    pub fn constrain_merge(
        &mut self,
        result: TypeVar,
        lhs: MonoType,
        rhs: MonoType,
        kind: MergeKind,
        span: Loc,
    ) {
        let c = Constraint::Merge {
            result,
            lhs,
            rhs,
            kind,
            span,
        };
        self.0.push(c);
    }

    /// Solves type and kind constraints in the order they were generated during inference.
    ///
    /// This mixed approach is preferred over separating unification and kind checking because:
    /// 1. **Preserves logical flow**: Constraints are solved in the same order as type inference,
    ///    maintaining the semantic relationship between type construction and validation
    /// 2. **Fail-fast**: Kind violations are caught immediately when the relevant substitution
    ///    becomes available, rather than deferring all kind checks to the end
    /// 3. **Better error locality**: Errors are reported at the point where the constraint
    ///    was actually generated, making debugging easier
    /// 4. **Theoretical soundness**: Matches constraint satisfaction solving where constraints
    ///    interact and should be resolved incrementally rather than in isolated phases
    ///
    /// For row polymorphism, this ensures that row variable bindings are validated against
    /// their Kind::Record constraints as soon as the unification occurs.
    ///
    /// **Important**: The unification algorithm applies current substitutions before binding
    /// new variables, ensuring that already-bound variables are properly resolved and
    /// preventing conflicting bindings to the same variable.
    pub fn solve(
        self,
        s: &mut Substitution,
        class_env: &mut TypeClassEnv,
    ) -> Result<(), Located<TypeErrors>> {
        for c in self.0 {
            match c {
                Constraint::Inst { from, to } => {
                    // Apply current substitutions to the variables
                    let from = from.apply(s);
                    let to = to.apply(s);

                    trace!("INSTANTIATE: {} -> {}", from, to);

                    // Fresh variables must not have a class
                    // If they have it is a compiler bug and therefore we panic
                    assert!(!class_env.contains_key(&to));

                    if let Some(from_classes) = class_env.get(&from).cloned() {
                        class_env.insert(to, from_classes);
                    }
                }
                Constraint::Class {
                    expected,
                    actual,
                    span,
                } => {
                    let actual = actual.apply_cow(s);

                    trace!("CLASS: {} :: {}", actual, expected);

                    actual
                        .constrain_class(expected, class_env)
                        .map_err(|e| (Errors::unit(e), span))?;
                }
                Constraint::Equal {
                    expected,
                    actual,
                    span,
                } => {
                    let lhs = expected.apply_cow(s);
                    let rhs = actual.apply_cow(s);

                    trace!("EQUALITY: {} ≈ {}", lhs, rhs);

                    lhs.try_unify(&rhs, s).map_err(|errors| ((errors, span)))?;
                }
                Constraint::Merge {
                    result,
                    lhs,
                    rhs,
                    kind: MergeKind::Left,
                    span,
                } => {
                    let lhs = lhs.apply_cow(s);
                    let rhs = rhs.apply_cow(s);

                    trace!("MERGE LEFT: {} ⊑ {}", lhs, rhs);

                    // TODO somehow return both errors if both are not records

                    let MonoType::Record(lhs) = lhs.as_ref() else {
                        return Err((
                            Errors::unit(TypeError::ExpectedRecord(lhs.into_owned())),
                            span,
                        ));
                    };

                    let MonoType::Record(rhs) = rhs.as_ref() else {
                        return Err((
                            Errors::unit(TypeError::ExpectedRecord(rhs.into_owned())),
                            span,
                        ));
                    };

                    let result_t = lhs.merge_left(&rhs).map_err(|e| (Errors::unit(e), span))?;

                    s.insert(result, MonoType::Record(Box::new(result_t)));
                }
                Constraint::Merge {
                    result,
                    lhs,
                    rhs,
                    kind: MergeKind::Right,
                    span,
                } => {
                    let lhs = lhs.apply_cow(s);
                    let rhs = rhs.apply_cow(s);

                    trace!("MERGE RIGHT: {} ⊒ {}", lhs, rhs);

                    // TODO somehow return both errors if both are not records

                    let MonoType::Record(lhs) = lhs.as_ref() else {
                        return Err((
                            Errors::unit(TypeError::ExpectedRecord(lhs.into_owned())),
                            span,
                        ));
                    };

                    let MonoType::Record(rhs) = rhs.as_ref() else {
                        return Err((
                            Errors::unit(TypeError::ExpectedRecord(rhs.into_owned())),
                            span,
                        ));
                    };

                    let result_t = lhs.merge_right(&rhs).map_err(|e| (Errors::unit(e), span))?;

                    s.insert(result, MonoType::Record(Box::new(result_t)));
                }
                Constraint::Merge {
                    result,
                    lhs,
                    rhs,
                    kind: MergeKind::Deep,
                    span,
                } => {
                    let lhs = lhs.apply_cow(s);
                    let rhs = rhs.apply_cow(s);

                    trace!("MERGE DEEP: {} ⊑ {}", lhs, rhs); // better symbol ?

                    // TODO somehow return both errors if both are not records

                    let MonoType::Record(lhs) = lhs.as_ref() else {
                        return Err((
                            Errors::unit(TypeError::ExpectedRecord(lhs.into_owned())),
                            span,
                        ));
                    };

                    let MonoType::Record(rhs) = rhs.as_ref() else {
                        return Err((
                            Errors::unit(TypeError::ExpectedRecord(rhs.into_owned())),
                            span,
                        ));
                    };

                    let result_t = lhs.merge_deep(&rhs).map_err(|e| (Errors::unit(e), span))?;

                    s.insert(result, MonoType::Record(Box::new(result_t)));
                }
            }
        }

        Ok(())
    }
}
