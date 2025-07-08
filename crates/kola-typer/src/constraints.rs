use kola_span::{Loc, Located};
use kola_utils::errors::Errors;
use log::trace;

use crate::{
    env::KindEnv,
    error::TypeErrors,
    substitute::{Substitutable, Substitution},
    types::{Kind, MonoType, Typed},
    unify::Unifiable,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint {
    Kind {
        expected: Kind,
        actual: MonoType,
        span: Loc,
    },
    Ty {
        expected: MonoType,
        actual: MonoType,
        span: Loc,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraints(Vec<Constraint>);

impl Constraints {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn constrain(&mut self, expected: MonoType, actual: MonoType, span: Loc) {
        let c = Constraint::Ty {
            expected,
            actual,
            span,
        };
        self.0.push(c);
    }

    // Same as constrains for debugging
    pub fn constrain_check(&mut self, expected: MonoType, actual: MonoType, span: Loc) {
        let c = Constraint::Ty {
            expected,
            actual,
            span,
        };
        self.0.push(c);
    }

    pub fn constrain_kind(&mut self, expected: Kind, actual: MonoType, span: Loc) {
        let c = Constraint::Kind {
            expected,
            actual,
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
        kind_env: &mut KindEnv,
    ) -> Result<(), Located<TypeErrors>> {
        for c in self.0 {
            match c {
                Constraint::Kind {
                    expected,
                    actual,
                    span,
                } => {
                    actual
                        .apply_cow(s)
                        .constrain(expected, kind_env)
                        .map_err(|e| (Errors::unit(e), span))?;
                }
                Constraint::Ty {
                    expected,
                    actual,
                    span,
                } => {
                    let lhs = expected.apply_cow(s);
                    let rhs = actual.apply_cow(s);

                    trace!("SOLVING: {} ≈ {}", lhs, rhs);

                    lhs.try_unify(&rhs, s).map_err(|errors| ((errors, span)))?;
                }
            }
        }

        Ok(())
    }
}
