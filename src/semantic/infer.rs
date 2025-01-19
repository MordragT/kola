use crate::{
    errors::Errors,
    syntax::{ast, Span, Spanned},
};

use super::{
    error::SemanticError,
    types::{Kind, MonoType, PolyType, TypeVar, Typed},
    Scopes, Substitutable, Substitution, Unifiable,
};

// https://blog.stimsina.com/post/implementing-a-hindley-milner-type-system-part-2

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint {
    Kind {
        expected: Kind,
        actual: MonoType,
        span: Span,
    },
    Ty {
        expected: MonoType,
        actual: MonoType,
        span: Span,
    },
}

type Error = Spanned<Errors<SemanticError>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraints(Vec<Constraint>);

impl Constraints {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn constrain(&mut self, expected: MonoType, actual: MonoType, span: Span) {
        let c = Constraint::Ty {
            expected,
            actual,
            span,
        };
        self.0.push(c);
    }

    pub fn constrain_kind(&mut self, expected: Kind, actual: MonoType, span: Span) {
        let c = Constraint::Kind {
            expected,
            actual,
            span,
        };
        self.0.push(c);
    }

    // TODO error handling do not propagate but keep trace of errors
    pub fn solve(self, s: &mut Substitution) -> Result<(), Error> {
        for c in self.0 {
            match c {
                Constraint::Kind {
                    expected,
                    actual,
                    span,
                } => {
                    dbg!(&expected, &actual, &actual.apply_cow(s));
                    actual
                        .apply_cow(s)
                        .constrain(expected, s)
                        .map_err(|e| e.with(span))?
                }
                Constraint::Ty {
                    expected,
                    actual,
                    span,
                } => expected
                    .try_unify(&actual, s)
                    .map_err(|errs| (errs, span))?,
            }
        }

        Ok(())
    }
}

pub trait Infer {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error>;

    fn solve(&self, s: &mut Substitution) -> Result<(), Error> {
        let mut cons = Constraints::new();
        let mut scopes = Scopes::new();

        self.infer(&mut cons, &mut scopes)?;

        cons.solve(s)
    }
}

// Unit rule
// -----------------------
// Γ ⊢ () : unit
impl Infer for ast::LiteralExpr {
    fn infer(&self, cons: &mut Constraints, _scopes: &mut Scopes) -> Result<MonoType, Error> {
        let actual = match self.inner() {
            &ast::Literal::Bool(_) => MonoType::BOOL,
            &ast::Literal::Char(_) => MonoType::CHAR,
            &ast::Literal::Num(_) => MonoType::NUM,
            &ast::Literal::Str(_) => MonoType::STR,
        };

        self.constrain(&actual, cons);
        Ok(actual)
    }
}

// Var rule to infer variable 'x'
// x : σ ∈ Γ   τ = inst(σ)
// -----------------------
// Γ ⊢ x : τ
impl Infer for ast::IdentExpr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        let t = scopes
            .try_lookup(self)
            .map_err(|e| e.with(self.span))?
            .instantiate();

        self.constrain(&t, cons);
        Ok(t)
    }
}

impl Infer for ast::ListExpr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        todo!()
    }
}

impl Infer for ast::RecordExpr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        todo!()
    }
}

// ∀rα. {l :: α | r} → α
impl Infer for ast::RecordSelect {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        todo!()
    }
}

// ∀rα. α → {r} → {l :: α | r}
impl Infer for ast::RecordExtend {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        todo!()
    }
}

// ∀rα. {l :: α | r} → {r}
impl Infer for ast::RecordRestrict {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        todo!()
    }
}

impl Infer for ast::RecordUpdate {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        todo!()
    }
}

// Abstraction rule
// τ = newvar()
// Γ, x : τ ⊢ e : τ'
// ___________________
// Γ ⊢ \x -> e : t -> t'
impl Infer for ast::UnaryOp {
    fn infer(&self, cons: &mut Constraints, _scopes: &mut Scopes) -> Result<MonoType, Error> {
        let t = match self.inner() {
            &ast::UnaryOpKind::Neg => MonoType::NUM,
            &ast::UnaryOpKind::Not => MonoType::BOOL,
        };

        let func = MonoType::func(t.clone(), t);

        self.constrain(&func, cons);
        Ok(func)
    }
}

// Application rule
// Γ ⊢ f : τ0
// Γ ⊢ x : τ1
// τ' = newvar()
// unify(τ0, τ1 -> τ')
// --------------------
// Γ ⊢ f x : τ'
impl Infer for ast::UnaryExpr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        let t0 = self.op.infer(cons, scopes)?;
        let t1 = self.target.infer(cons, scopes)?;

        let t_prime = MonoType::variable();

        cons.constrain(t0, MonoType::func(t1, t_prime.clone()), self.span);

        self.constrain(&t_prime, cons);
        Ok(t_prime)
    }
}

impl Infer for ast::BinaryOp {
    fn infer(&self, cons: &mut Constraints, _scopes: &mut Scopes) -> Result<MonoType, Error> {
        let (t, t_prime) = match self.inner() {
            ast::BinaryOpKind::Add => {
                let t = MonoType::variable();
                cons.constrain_kind(Kind::Addable, t.clone(), self.span);
                (t, MonoType::NUM)
            }
            ast::BinaryOpKind::Sub
            | ast::BinaryOpKind::Mul
            | ast::BinaryOpKind::Div
            | ast::BinaryOpKind::Rem => (MonoType::NUM, MonoType::NUM),
            // Comparison
            ast::BinaryOpKind::Less
            | ast::BinaryOpKind::Greater
            | ast::BinaryOpKind::LessEq
            | ast::BinaryOpKind::GreaterEq => {
                let t = MonoType::variable();
                cons.constrain_kind(Kind::Comparable, t.clone(), self.span);
                (t, MonoType::BOOL)
            }
            // Logical
            ast::BinaryOpKind::And | ast::BinaryOpKind::Or | ast::BinaryOpKind::Xor => {
                (MonoType::BOOL, MonoType::BOOL)
            }
            // Equality
            ast::BinaryOpKind::Eq | ast::BinaryOpKind::NotEq => {
                let t = MonoType::variable();
                cons.constrain_kind(Kind::Equatable, t.clone(), self.span);
                (t, MonoType::BOOL)
            }
        };

        let func = MonoType::func(t.clone(), MonoType::func(t, t_prime));

        self.constrain(&func, cons);
        Ok(func)
    }
}

// Application rule
// Γ ⊢ f : τ0
// Γ ⊢ x : τ1
// τ' = newvar()
// unify(τ0, τ1 -> τ')
// --------------------
// Γ ⊢ f x : τ'
impl Infer for ast::BinaryExpr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        let t0 = self.op.infer(cons, scopes)?;
        let lhs = self.left.infer(cons, scopes)?;
        let rhs = self.right.infer(cons, scopes)?;
        let t_prime = MonoType::variable();

        let func = MonoType::func(lhs, MonoType::func(rhs, t_prime.clone()));
        cons.constrain(t0, func, self.span);

        self.constrain(&t_prime, cons);
        Ok(t_prime)
    }
}

// Generalization
// Γ'(τ) quantifies all monotype variables not bound in Γ

// Let rule
// Γ ⊢ e0 : τ
// Γ, x : Γ'(τ) ⊢ e1 : τ'
// --------------------
// Γ ⊢ let x = e0 in e1 : τ'
impl Infer for ast::LetExpr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        let t = TypeVar::branch(|| self.value.infer(cons, scopes))?;

        scopes.enter();

        let pty = t.generalize(&scopes.bound_vars());
        scopes.insert(self.name.name.clone(), pty);

        let t_prime = self.inside.infer(cons, scopes)?;

        scopes.exit();

        self.constrain(&t_prime, cons);
        Ok(t_prime)
    }
}

// If
// Γ ⊢ predicate : Bool
// Γ ⊢ e0 : τ
// Γ ⊢ e1 : τ
// --------------------------
// Γ ⊢ if predicate then e0 else e1 : τ
impl Infer for ast::IfExpr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        let t0 = self.predicate.infer(cons, scopes)?;

        cons.constrain(MonoType::BOOL, t0, self.span);

        let then = self.then.infer(cons, scopes)?;
        let or = self.or.infer(cons, scopes)?;

        cons.constrain(then.clone(), or, self.span);

        self.constrain(&then, cons);
        Ok(then)
    }
}

impl Infer for ast::CaseExpr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        todo!()
    }
}

// Abstraction rule
// τ = newvar()
// Γ, x : τ ⊢ e : τ'
// ___________________
// Γ ⊢ \x -> e : t -> t'
impl Infer for ast::FuncExpr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        let t = MonoType::variable();

        scopes.enter();

        scopes.insert(self.param.inner().clone(), PolyType::from(t.clone()));
        let t_prime = self.body.infer(cons, scopes)?;

        scopes.exit();

        let func = MonoType::func(t, t_prime);

        self.constrain(&func, cons);
        Ok(func)
    }
}

// Application rule
// Γ ⊢ f : τ0
// Γ ⊢ x : τ1
// τ' = newvar()
// unify(τ0, τ1 -> τ')
// --------------------
// Γ ⊢ f x : τ'
impl Infer for ast::CallExpr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        let t0 = self.func.infer(cons, scopes)?;
        let t1 = self.arg.infer(cons, scopes)?;
        let t_prime = MonoType::variable();

        cons.constrain(t0, MonoType::func(t1, t_prime.clone()), self.span);

        self.constrain(&t_prime, cons);
        Ok(t_prime)
    }
}

impl Infer for ast::Expr {
    fn infer(&self, cons: &mut Constraints, scopes: &mut Scopes) -> Result<MonoType, Error> {
        match self {
            Self::Error(e) => Err((Errors::new(), e.span)),
            Self::Literal(l) => l.infer(cons, scopes),
            Self::Ident(i) => i.infer(cons, scopes),
            Self::List(l) => l.infer(cons, scopes),
            Self::Record(r) => r.infer(cons, scopes),
            Self::RecordSelect(r) => r.infer(cons, scopes),
            Self::RecordExtend(r) => r.infer(cons, scopes),
            Self::RecordRestrict(r) => r.infer(cons, scopes),
            Self::RecordUpdate(r) => r.infer(cons, scopes),
            Self::Unary(u) => u.infer(cons, scopes),
            Self::Binary(b) => b.infer(cons, scopes),
            Self::Let(l) => l.infer(cons, scopes),
            Self::If(i) => i.infer(cons, scopes),
            Self::Case(c) => c.infer(cons, scopes),
            Self::Func(f) => f.infer(cons, scopes),
            Self::Call(c) => c.infer(cons, scopes),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        semantic::{error::SemanticError, types::MonoType, Infer, Substitution},
        syntax::{ast::*, node::Node, Span},
    };

    fn node<T>(t: T) -> Node<T> {
        Node::new(t, Span::new(0, 0))
    }

    #[test]
    fn literal() {
        let mut s = Substitution::empty();

        let mut lit = node(Literal::Num(10.0));
        lit.solve(&mut s).unwrap();
        s.apply(&mut lit);

        assert_eq!(lit.ty(), &MonoType::NUM);
    }

    #[test]
    fn unary() {
        let mut s = Substitution::empty();

        let mut unary = node(Unary {
            op: node(UnaryOpKind::Neg),
            target: node(Literal::Num(10.0)).into(),
        });

        unary.solve(&mut s).unwrap();
        s.apply(&mut unary);

        assert_eq!(unary.ty(), &MonoType::NUM);

        s.clear();

        let unary = node(Unary {
            op: node(UnaryOpKind::Not),
            target: node(Literal::Num(10.0)).into(),
        });

        let (errors, _) = unary.solve(&mut s).unwrap_err();

        assert_eq!(
            errors[0],
            SemanticError::CannotUnify {
                expected: MonoType::BOOL,
                actual: MonoType::NUM
            }
        );
    }

    #[test]
    fn binary() {
        let mut s = Substitution::empty();

        let binary = node(Binary {
            op: node(BinaryOpKind::Eq),
            left: node(Literal::Bool(true)).into(),
            right: node(Literal::Num(10.0)).into(),
        });

        let (errors, _) = binary.solve(&mut s).unwrap_err();

        assert_eq!(
            errors[0],
            SemanticError::CannotUnify {
                expected: MonoType::BOOL,
                actual: MonoType::NUM
            }
        );
    }

    #[test]
    fn let_() {
        let mut s = Substitution::empty();

        let mut let_ = node(Let {
            name: Name {
                name: "x".into(),
                span: Span::new(0, 0),
            },
            value: node(Literal::Num(10.0)).into(),
            inside: node(Symbol::from("x")).into(),
        });

        let_.solve(&mut s).unwrap();
        s.apply(&mut let_);

        assert_eq!(let_.ty(), &MonoType::NUM)
    }

    #[test]
    fn if_() {
        let mut s = Substitution::empty();

        let mut if_ = node(If {
            predicate: node(Literal::Bool(true)).into(),
            then: node(Literal::Num(5.0)).into(),
            or: node(Literal::Num(10.0)).into(),
        });

        if_.solve(&mut s).unwrap();
        s.apply(&mut if_);

        assert_eq!(if_.ty(), &MonoType::NUM);

        s.clear();

        let if_ = node(If {
            predicate: node(Literal::Bool(true)).into(),
            then: node(Literal::Num(5.0)).into(),
            or: node(Literal::Char('x')).into(),
        });

        let (errors, _) = if_.solve(&mut s).unwrap_err();

        assert_eq!(
            errors[0],
            SemanticError::CannotUnify {
                expected: MonoType::NUM,
                actual: MonoType::CHAR,
            }
        );
    }
}
