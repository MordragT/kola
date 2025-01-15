use std::ops::ControlFlow;

use crate::{
    errors::Errors,
    syntax::{
        ast,
        visit::{Visitable, Visitor},
        Span, Spanned,
    },
};

use super::{
    error::SemanticError,
    types::{MonoType, PolyType, TypeVar},
    Scopes, Substitutable, Substitution, Unifiable,
};

type Error = Spanned<Errors<SemanticError>>;

pub trait Inferable: Visitable + Sized {
    fn infer(&mut self, s: &mut Substitution) -> Result<(), Error> {
        let mut inferer = Inferer::new(s);
        match self.visit_by(&mut inferer) {
            ControlFlow::Continue(()) => {
                inferer.substitution.apply(self);
                Ok(())
            }
            ControlFlow::Break(e) => Err(e),
        }
    }
}

impl<T: Visitable + Sized> Inferable for T {}

struct Inferer<'s> {
    substitution: &'s mut Substitution,
    scopes: Scopes,
}

impl<'s> Inferer<'s> {
    fn new(substitution: &'s mut Substitution) -> Self {
        Self {
            substitution,
            scopes: Scopes::new(),
        }
    }

    fn branch<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.scopes.enter();
        let result = f(self);
        self.scopes.exit();
        result
    }

    fn unify_at<L, R>(&mut self, lhs: &L, rhs: &R, span: Span) -> ControlFlow<Error>
    where
        L: Unifiable<R>,
    {
        match lhs.try_unify(rhs, &mut self.substitution) {
            Err(e) => ControlFlow::Break((e, span)),
            Ok(()) => ControlFlow::Continue(()),
        }
    }

    fn type_of<'a>(&self, expr: &'a ast::Expr) -> ControlFlow<Error, &'a MonoType> {
        match expr.ty() {
            Err(e) => ControlFlow::Break((Errors::new(), e.span)),
            Ok(t) => ControlFlow::Continue(t),
        }
    }

    fn lookup(&self, ident: &ast::IdentExpr) -> ControlFlow<Error, &PolyType> {
        match self.scopes.try_get(ident) {
            Err(e) => ControlFlow::Break((Errors::from(vec![e]), ident.span)),
            Ok(pt) => ControlFlow::Continue(pt),
        }
    }
}

impl Visitor for Inferer<'_> {
    type BreakValue = Error;

    // Unit rule
    // -----------------------
    // Γ ⊢ () : unit
    fn visit_literal(&mut self, literal: &ast::LiteralExpr) -> ControlFlow<Self::BreakValue> {
        let actual = match literal.inner() {
            &ast::Literal::Bool(_) => MonoType::BOOL,
            &ast::Literal::Char(_) => MonoType::CHAR,
            &ast::Literal::Num(_) => MonoType::NUM,
            &ast::Literal::Str(_) => MonoType::STR,
        };

        self.unify_at(literal.ty(), &actual, literal.span)?;
        ControlFlow::Continue(())
    }

    // Var rule to infer variable 'x'
    // x : σ ∈ Γ   τ = inst(σ)
    // -----------------------
    // Γ ⊢ x : τ
    fn visit_ident(&mut self, ident: &ast::IdentExpr) -> ControlFlow<Self::BreakValue> {
        let t = self.lookup(ident)?.instantiate();

        self.unify_at(ident.ty(), &t, ident.span)?;
        ControlFlow::Continue(())
    }

    fn visit_list(&mut self, list: &ast::ListExpr) -> ControlFlow<Self::BreakValue> {
        // if let Some((first, rest)) = self.0.values.split_first() {
        //     let t0 = first.infer_with(ctx)?;
        //     todo!()
        // } else {
        //     todo!()
        // }
        todo!()
    }

    fn visit_record(&mut self, record: &ast::RecordExpr) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    // ∀rα. {l :: α | r} → α
    fn visit_record_select(
        &mut self,
        record_select: &ast::RecordSelectExpr,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    // ∀rα. α → {r} → {l :: α | r}
    fn visit_record_extend(
        &mut self,
        record_extend: &ast::RecordExtendExpr,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    // ∀rα. {l :: α | r} → {r}
    fn visit_record_restrict(
        &mut self,
        record_restrict: &ast::RecordRestrictExpr,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    fn visit_record_update(
        &mut self,
        record_update: &ast::RecordUpdateExpr,
    ) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    // Abstraction rule
    // τ = newvar()
    // Γ, x : τ ⊢ e : τ'
    // ___________________
    // Γ ⊢ \x -> e : t -> t'
    fn visit_unary_op(&mut self, unary_op: &ast::UnaryOp) -> ControlFlow<Self::BreakValue> {
        let t = match unary_op.inner() {
            &ast::UnaryOpKind::Neg => MonoType::NUM,
            &ast::UnaryOpKind::Not => MonoType::BOOL,
        };

        self.unify_at(unary_op.ty(), &MonoType::func(t.clone(), t), unary_op.span)?;
        ControlFlow::Continue(())
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn visit_unary(&mut self, unary: &ast::UnaryExpr) -> ControlFlow<Self::BreakValue> {
        self.visit_unary_op(&unary.op)?;
        self.visit_expr(&unary.target)?;

        // We use this as needle for later t_prime unification where this must be a function therefore apply
        let t0 = unary.op.ty().apply_cow(&mut self.substitution);
        let t1 = self.type_of(&unary.target)?;

        self.unify_at(
            t0.as_ref(),
            &MonoType::func(t1.clone(), MonoType::variable()),
            unary.span,
        )?;

        let t_prime = &t0.as_func().unwrap().ret;

        self.unify_at(unary.ty(), t_prime, unary.span)?;
        ControlFlow::Continue(())
    }

    fn visit_binary_op(&mut self, binary_op: &ast::BinaryOp) -> ControlFlow<Self::BreakValue> {
        let t = MonoType::variable();
        let t_prime = match binary_op.inner() {
            &ast::BinaryOpKind::Add => MonoType::NUM,
            &ast::BinaryOpKind::Sub => MonoType::NUM,
            &ast::BinaryOpKind::Mul => MonoType::NUM,
            &ast::BinaryOpKind::Div => MonoType::NUM,
            &ast::BinaryOpKind::Rem => MonoType::NUM,
            // Comparison
            &ast::BinaryOpKind::Less => MonoType::BOOL,
            &ast::BinaryOpKind::Greater => MonoType::BOOL,
            &ast::BinaryOpKind::LessEq => MonoType::BOOL,
            &ast::BinaryOpKind::GreaterEq => MonoType::BOOL,
            // Logical
            &ast::BinaryOpKind::And => MonoType::BOOL,
            &ast::BinaryOpKind::Or => MonoType::BOOL,
            &ast::BinaryOpKind::Xor => MonoType::BOOL,
            // Equality
            &ast::BinaryOpKind::Eq => MonoType::BOOL,
            &ast::BinaryOpKind::NotEq => MonoType::BOOL,
        };

        let func = MonoType::func(t.clone(), MonoType::func(t, t_prime));

        self.unify_at(binary_op.ty(), &func, binary_op.span)?;
        ControlFlow::Continue(())
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn visit_binary(&mut self, binary: &ast::BinaryExpr) -> ControlFlow<Self::BreakValue> {
        self.visit_binary_op(&binary.op)?;
        self.visit_expr(&binary.left)?;
        self.visit_expr(&binary.right)?;

        // We use this as needle for later t_prime unification where this must be a function therefore apply
        let t0 = binary.op.ty().apply_cow(&mut self.substitution);
        let lhs = self.type_of(&binary.left)?;
        let rhs = self.type_of(&binary.right)?;

        let func = MonoType::func(
            lhs.clone(),
            MonoType::func(rhs.clone(), MonoType::variable()),
        );
        self.unify_at(t0.as_ref(), &func, binary.span)?;

        let t_prime = &t0.as_func().unwrap().ret.as_func().unwrap().ret;

        self.unify_at(binary.ty(), t_prime, binary.span)?;
        ControlFlow::Continue(())
    }

    // Generalization
    // Γ'(τ) quantifies all monotype variables not bound in Γ

    // Let rule
    // Γ ⊢ e0 : τ
    // Γ, x : Γ'(τ) ⊢ e1 : τ'
    // --------------------
    // Γ ⊢ let x = e0 in e1 : τ'
    fn visit_let(&mut self, let_: &ast::LetExpr) -> ControlFlow<Self::BreakValue> {
        TypeVar::branch(|| self.visit_expr(&let_.value))?;

        let t = self.type_of(&let_.value)?.apply_cow(&mut self.substitution); // TODO is this correct ? Do I need to substitute before generalization

        self.branch(|ctx| {
            let pty = t.generalize(&ctx.scopes.bound_vars());
            ctx.scopes.insert(let_.name.name.clone(), pty);
            ctx.visit_expr(&let_.inside)
        })?;

        let t_prime = self.type_of(&let_.inside)?;

        self.unify_at(let_.ty(), t_prime, let_.span)?;
        ControlFlow::Continue(())
    }

    // If
    // Γ ⊢ predicate : Bool
    // Γ ⊢ e0 : τ
    // Γ ⊢ e1 : τ
    // --------------------------
    // Γ ⊢ if predicate then e0 else e1 : τ
    fn visit_if(&mut self, if_: &ast::IfExpr) -> ControlFlow<Self::BreakValue> {
        self.visit_expr(&if_.predicate)?;

        let t0 = self.type_of(&if_.predicate)?;
        self.unify_at(t0, &MonoType::BOOL, if_.span)?;

        self.visit_expr(&if_.then)?;
        self.visit_expr(&if_.or)?;

        let then = self.type_of(&if_.then)?;
        let or = self.type_of(&if_.or)?;

        self.unify_at(then, or, if_.span)?;

        self.unify_at(if_.ty(), then, if_.span)?;
        ControlFlow::Continue(())
    }

    fn visit_case(&mut self, case: &ast::CaseExpr) -> ControlFlow<Self::BreakValue> {
        todo!()
    }

    // Abstraction rule
    // τ = newvar()
    // Γ, x : τ ⊢ e : τ'
    // ___________________
    // Γ ⊢ \x -> e : t -> t'
    fn visit_func(&mut self, func: &ast::FuncExpr) -> ControlFlow<Self::BreakValue> {
        let t = MonoType::variable();
        self.branch(|ctx| {
            ctx.scopes
                .insert(func.param.inner().clone(), PolyType::from(t.clone()));
            ctx.visit_expr(&func.body)
        })?;

        let t_prime = self.type_of(&func.body)?;

        self.unify_at(func.ty(), &MonoType::func(t, t_prime.clone()), func.span)?;
        ControlFlow::Continue(())
    }

    // Application rule
    // Γ ⊢ f : τ0
    // Γ ⊢ x : τ1
    // τ' = newvar()
    // unify(τ0, τ1 -> τ')
    // --------------------
    // Γ ⊢ f x : τ'
    fn visit_call(&mut self, call: &ast::CallExpr) -> ControlFlow<Self::BreakValue> {
        self.visit_ident(&call.func)?;
        self.visit_expr(&call.arg)?;

        // We use this as needle for later t_prime unification where this must be a function therefore apply
        let t0 = call.func.ty().apply_cow(&mut self.substitution);
        let t1 = self.type_of(&call.arg)?;

        self.unify_at(
            t0.as_ref(),
            &MonoType::func(t1.clone(), MonoType::variable()),
            call.span,
        )?;

        let t_prime = &t0.as_func().unwrap().ret;

        self.unify_at(call.ty(), t_prime, call.span)?;
        ControlFlow::Continue(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        semantic::{error::SemanticError, types::MonoType, Inferable, Substitution},
        syntax::{ast::*, node::Node, Span},
    };

    fn node<T>(t: T) -> Node<T> {
        Node::new(t, Span::new(0, 0))
    }

    #[test]
    fn literal() {
        let mut s = Substitution::empty();

        let mut lit = node(Literal::Num(10.0));
        lit.infer(&mut s).unwrap();

        assert_eq!(lit.ty(), &MonoType::NUM);
    }

    #[test]
    fn unary() {
        let mut s = Substitution::empty();

        let mut unary = node(Unary {
            op: node(UnaryOpKind::Neg),
            target: node(Literal::Num(10.0)).into(),
        });

        unary.infer(&mut s).unwrap();

        assert_eq!(unary.ty(), &MonoType::NUM);

        s.clear();

        let mut unary = node(Unary {
            op: node(UnaryOpKind::Not),
            target: node(Literal::Num(10.0)).into(),
        });

        let (errors, _) = unary.infer(&mut s).unwrap_err();

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

        let mut binary = node(Binary {
            op: node(BinaryOpKind::Eq),
            left: node(Literal::Bool(true)).into(),
            right: node(Literal::Num(10.0)).into(),
        });

        let (errors, _) = binary.infer(&mut s).unwrap_err();

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

        let_.infer(&mut s).unwrap();

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

        if_.infer(&mut s).unwrap();

        assert_eq!(if_.ty(), &MonoType::NUM);

        s.clear();

        let mut if_ = node(If {
            predicate: node(Literal::Bool(true)).into(),
            then: node(Literal::Num(5.0)).into(),
            or: node(Literal::Char('x')).into(),
        });

        let (errors, _) = if_.infer(&mut s).unwrap_err();

        assert_eq!(
            errors[0],
            SemanticError::CannotUnify {
                expected: MonoType::NUM,
                actual: MonoType::CHAR,
            }
        );
    }
}
