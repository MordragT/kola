use std::ops::ControlFlow;

use crate::{
    source::Source,
    syntax::{
        ast,
        visit::{Visit, Visitable},
    },
};

use super::{
    error::InferReport,
    types::{MonoType, PolyType, TypeVar},
    Constraints, Scopes, Substitutable, Unifier, Unify,
};

pub struct Inferer {
    pub unifier: Unifier,
    pub scopes: Scopes,
    pub constraints: Constraints,
}

impl Inferer {
    pub fn new(source: &Source) -> Self {
        Self {
            unifier: Unifier::new(source),
            scopes: Scopes::new(),
            constraints: Constraints::new(),
        }
    }

    pub fn clear(&mut self) {
        self.unifier.clear();
        self.scopes.clear();
        self.constraints.clear();
    }

    pub fn branch<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.scopes.enter();
        let result = f(self);
        self.scopes.exit();
        result
    }

    pub fn infer<N>(&mut self, node: &mut N) -> Result<(), InferReport>
    where
        N: Visitable,
    {
        match node.visit_by(self) {
            ControlFlow::Break(report) => Err(report),
            ControlFlow::Continue(_) => {
                assert!(!self.unifier.has_errors());
                self.unifier.substitution.apply(node);
                Ok(())
            }
        }

        // let mut ctx = Context::new(source);

        // let ty = self
        //     .infer_with(&mut ctx)?
        //     .apply(&mut ctx.substitution, &mut ctx.cache);

        // Ok(ty)

        // match self.infer_with(&mut ctx) {
        //     Ok(ty) => {
        //         if ctx.has_errors() {
        //             Err(ctx.take_errors())
        //         }

        //         ty.apply_mut(&mut ctx.substitution, &mut ctx.cache);

        //         Ok(ty)
        //     }
        //     Err(err) => {
        //         ctx.error(err);
        //         Err(ctx.take_errors())
        //     }
        // }

        // if self.has_errors() {
        //     Err(self.take_errors())
        // } else {

        // }
    }

    fn type_of<'a>(&self, expr: &'a ast::Expr) -> ControlFlow<InferReport, &'a MonoType> {
        match expr.ty() {
            Err(e) => ControlFlow::Break(InferReport::new(
                Vec::new(),
                e.span,
                self.unifier.named_source(),
            )),
            Ok(t) => ControlFlow::Continue(t),
        }
    }

    fn try_get(&self, ident: &ast::IdentExpr) -> ControlFlow<InferReport, &PolyType> {
        match self.scopes.try_get(ident) {
            Err(e) => ControlFlow::Break(e.with(ident.span, self.unifier.named_source())),
            Ok(pt) => ControlFlow::Continue(pt),
        }
    }
}

impl Visit for Inferer {
    type BreakValue = InferReport;

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
        literal
            .ty()
            .try_unify(&actual, literal.span, &mut self.unifier)?;
        ControlFlow::Continue(())
    }

    // Var rule to infer variable 'x'
    // x : σ ∈ Γ   τ = inst(σ)
    // -----------------------
    // Γ ⊢ x : τ
    fn visit_ident(&mut self, ident: &ast::IdentExpr) -> ControlFlow<Self::BreakValue> {
        let t = self.try_get(ident)?.instantiate();

        ident.ty().try_unify(&t, ident.span, &mut self.unifier)?;
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

        unary_op.ty().try_unify(
            &MonoType::func(t.clone(), t),
            unary_op.span,
            &mut self.unifier,
        )?;
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
        let t0 = unary.op.ty().apply_cow(&mut self.unifier.substitution);
        let t1 = self.type_of(&unary.target)?;

        t0.try_unify(
            &MonoType::func(t1.clone(), MonoType::variable()),
            unary.span,
            &mut self.unifier,
        )?;

        let t_prime = &t0.as_func().unwrap().ret;

        unary
            .ty()
            .try_unify(t_prime, unary.span, &mut self.unifier)?;
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

        binary_op
            .ty()
            .try_unify(&func, binary_op.span, &mut self.unifier)?;
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
        let t0 = binary.op.ty().apply_cow(&mut self.unifier.substitution);
        let lhs = self.type_of(&binary.left)?;
        let rhs = self.type_of(&binary.right)?;

        let func = MonoType::func(
            lhs.clone(),
            MonoType::func(rhs.clone(), MonoType::variable()),
        );
        t0.try_unify(&func, binary.span, &mut self.unifier)?;

        let t_prime = &t0.as_func().unwrap().ret.as_func().unwrap().ret;

        binary
            .ty()
            .try_unify(t_prime, binary.span, &mut self.unifier)?;
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

        let t = self
            .type_of(&let_.value)?
            .apply_cow(&mut self.unifier.substitution); // TODO is this correct ? Do I need to substitute before generalization

        self.branch(|ctx| {
            let pty = t.generalize(&ctx.scopes.bound_vars());
            ctx.scopes.insert(let_.name.name.clone(), pty);
            ctx.visit_expr(&let_.inside)
        })?;

        let t_prime = self.type_of(&let_.inside)?;

        let_.ty().try_unify(t_prime, let_.span, &mut self.unifier)?;
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
        t0.try_unify(&MonoType::BOOL, if_.span, &mut self.unifier)?;

        self.visit_expr(&if_.then)?;
        self.visit_expr(&if_.or)?;

        let then = self.type_of(&if_.then)?;
        let or = self.type_of(&if_.or)?;

        then.try_unify(&or, if_.span, &mut self.unifier)?;

        if_.ty().try_unify(then, if_.span, &mut self.unifier)?;
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

        func.ty().try_unify(
            &MonoType::func(t, t_prime.clone()),
            func.span,
            &mut self.unifier,
        )?;
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
        let t0 = call.func.ty().apply_cow(&mut self.unifier.substitution);
        let t1 = self.type_of(&call.arg)?;

        t0.try_unify(
            &MonoType::func(t1.clone(), MonoType::variable()),
            call.span,
            &mut self.unifier,
        )?;

        let t_prime = &t0.as_func().unwrap().ret;

        call.ty().try_unify(t_prime, call.span, &mut self.unifier)?;
        ControlFlow::Continue(())
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::{
//         semantic::{error::InferError, types::MonoType, Context, Infer},
//         syntax::{ast::*, Span},
//     };

//     #[test]
//     fn literal() {
//         let mut ctx = Context::new();

//         let lit = Literal::Num(10.0);
//         let ty = lit.infer_with(&mut ctx).unwrap();

//         assert_eq!(ty, MonoType::NUM);
//     }

//     #[test]
//     fn unary() {
//         let mut ctx = Context::new();

//         let span = Span::new(0, 0);
//         let target = Expr::Literal(Literal::Num(10.0), span);
//         let unary = UnaryExpr {
//             op: UnaryOp::Neg,
//             target: Box::new(target.clone()),
//         };

//         let ty = unary.infer_with(&mut ctx).unwrap();

//         assert_eq!(ty, MonoType::NUM);

//         let unary = UnaryExpr {
//             op: UnaryOp::Not,
//             target: Box::new(target),
//         };

//         let errors = unary.infer_with(&mut ctx).unwrap_err();

//         assert_eq!(
//             errors.related[0],
//             InferError::CannotUnify {
//                 expected: MonoType::BOOL,
//                 actual: MonoType::NUM
//             }
//         );
//     }

//     #[test]
//     fn binary() {
//         let mut ctx = Context::new();

//         let lit = |lit| Box::new(Expr::Literal(lit, Span::new(0, 0)));

//         let binary = BinaryExpr {
//             op: BinaryOp::Eq,
//             left: lit(Literal::Bool(true)),
//             right: lit(Literal::Num(10.0)),
//         };

//         let errors = binary.infer_with(&mut ctx).unwrap_err();

//         assert_eq!(
//             errors.related[0],
//             InferError::CannotUnify {
//                 expected: MonoType::BOOL,
//                 actual: MonoType::NUM
//             }
//         );
//     }

//     #[test]
//     fn let_() {
//         let mut ctx = Context::new();

//         let span = Span::new(0, 0);

//         let let_ = LetExpr {
//             name: "x".into(),
//             value: Box::new(Expr::Literal(Literal::Num(10.0), span)),
//             inside: Box::new(Expr::Ident("x".into(), span)),
//         };

//         let ty = let_.infer_with(&mut ctx).unwrap();

//         assert_eq!(ty, MonoType::NUM)
//     }

//     #[test]
//     fn if_() {
//         let mut ctx = Context::new();

//         let lit = |lit| Box::new(Expr::Literal(lit, Span::new(0, 0)));

//         let if_ = IfExpr {
//             predicate: lit(Literal::Bool(true)),
//             then: lit(Literal::Num(5.0)),
//             or: lit(Literal::Num(10.0)),
//         };

//         let ty = if_.infer_with(&mut ctx).unwrap();

//         assert_eq!(ty, MonoType::NUM);
//         ctx.clear();

//         let if_ = IfExpr {
//             predicate: lit(Literal::Bool(true)),
//             then: lit(Literal::Num(5.0)),
//             or: lit(Literal::Char('x')),
//         };

//         let errors = if_.infer_with(&mut ctx).unwrap_err();

//         assert_eq!(
//             errors.related[0],
//             InferError::CannotUnify {
//                 expected: MonoType::NUM,
//                 actual: MonoType::CHAR,
//             }
//         );
//     }
// }
