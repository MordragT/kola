use crate::{
    source::Source,
    syntax::{ast::*, Spanned},
};

use super::{
    error::InferReport,
    types::{MonoType, PolyType, TypeVar},
    Context, Substitutable, Unify,
};

type Result = std::result::Result<MonoType, InferReport>;

pub trait Infer {
    // TODO when do I want to propagate errors ?
    fn infer_with(&self, ctx: &mut Context) -> Result;

    fn infer(&self, source: Source) -> Result {
        let mut ctx = Context::new(source);

        let ty = self
            .infer_with(&mut ctx)?
            .apply(&mut ctx.substitution, &mut ctx.cache);

        assert!(!ctx.has_errors());

        Ok(ty)

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
}

// Unit rule
// -----------------------
// Γ ⊢ () : unit
impl Infer for Spanned<Literal> {
    fn infer_with(&self, _: &mut Context) -> Result {
        let ty = match self.0 {
            Literal::Bool(_) => MonoType::BOOL,
            Literal::Char(_) => MonoType::CHAR,
            Literal::Num(_) => MonoType::NUM,
            Literal::Str(_) => MonoType::STR,
        };
        Ok(ty)
    }
}

// Var rule to infer variable 'x'
// x : σ ∈ Γ   τ = inst(σ)
// -----------------------
// Γ ⊢ x : τ
impl Infer for Spanned<Ident> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        let t = ctx
            .scopes
            .try_get(&self.0)
            .map_err(|e| e.with(self.1, ctx.named_source()))?
            .instantiate();
        Ok(t)
    }
}

impl Infer for Spanned<List> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        if let Some((first, rest)) = self.0.values.split_first() {
            let t0 = first.infer_with(ctx)?;
            todo!()
        } else {
            todo!()
        }
    }
}

impl Infer for Spanned<Record> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

// ∀rα. {l :: α | r} → α
impl Infer for Spanned<RecordSelect> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

// ∀rα. α → {r} → {l :: α | r}
impl Infer for Spanned<RecordExtend> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

// ∀rα. {l :: α | r} → {r}
impl Infer for Spanned<RecordRestrict> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

impl Infer for Spanned<RecordUpdate> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

// Abstraction rule
// τ = newvar()
// Γ, x : τ ⊢ e : τ'
// ___________________
// Γ ⊢ \x -> e : t -> t'
impl Infer for UnaryOp {
    fn infer_with(&self, _: &mut Context) -> Result {
        let t = match self {
            Self::Neg => MonoType::NUM,
            Self::Not => MonoType::BOOL,
        };

        Ok(MonoType::func(t.clone(), t))
    }
}

// Application rule
// Γ ⊢ f : τ0
// Γ ⊢ x : τ1
// τ' = newvar()
// unify(τ0, τ1 -> τ')
// --------------------
// Γ ⊢ f x : τ'
impl Infer for Spanned<UnaryExpr> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        let t0 = self.0.op.infer_with(ctx)?;
        let t1 = self.0.target.infer_with(ctx)?;

        let unified = t0.try_unify(&MonoType::func(t1, MonoType::variable()), self.1, ctx)?;

        let t_prime = unified.into_func().unwrap().ret;

        Ok(t_prime)
    }
}

impl Infer for BinaryOp {
    fn infer_with(&self, _: &mut Context) -> Result {
        let t = MonoType::variable();
        let t_prime = match self {
            Self::Add => MonoType::NUM,
            Self::Sub => MonoType::NUM,
            Self::Mul => MonoType::NUM,
            Self::Div => MonoType::NUM,
            Self::Rem => MonoType::NUM,
            // Comparison
            Self::Less => MonoType::BOOL,
            Self::Greater => MonoType::BOOL,
            Self::LessEq => MonoType::BOOL,
            Self::GreaterEq => MonoType::BOOL,
            // Logical
            Self::And => MonoType::BOOL,
            Self::Or => MonoType::BOOL,
            Self::Xor => MonoType::BOOL,
            // Equality
            Self::Eq => MonoType::BOOL,
            Self::NotEq => MonoType::BOOL,
        };

        Ok(MonoType::func(t.clone(), MonoType::func(t, t_prime)))
    }
}

// Application rule
// Γ ⊢ f : τ0
// Γ ⊢ x : τ1
// τ' = newvar()
// unify(τ0, τ1 -> τ')
// --------------------
// Γ ⊢ f x : τ'
impl Infer for Spanned<BinaryExpr> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        let t0 = self.0.op.infer_with(ctx)?;

        let lhs = self.0.left.infer_with(ctx)?;
        let rhs = self.0.right.infer_with(ctx)?;

        let unified = t0.try_unify(
            &MonoType::func(lhs, MonoType::func(rhs, MonoType::variable())),
            self.1,
            ctx,
        )?;

        let t_prime = unified.into_func().unwrap().ret.into_func().unwrap().ret;

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
impl Infer for Spanned<LetExpr> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        let t = TypeVar::branch(|| self.0.value.infer_with(ctx))?
            .apply(&mut ctx.substitution, &mut ctx.cache); // TODO is this correct ? Do I need to substitute before generalization

        let t_prime = ctx.branch(|ctx| {
            let pty = t.generalize(&ctx.scopes.bound_vars());
            ctx.scopes.insert(self.0.name.clone(), pty);
            self.0.inside.infer_with(ctx)
        })?;

        Ok(t_prime)
    }
}

// If
// Γ ⊢ predicate : Bool
// Γ ⊢ e0 : τ
// Γ ⊢ e1 : τ
// --------------------------
// Γ ⊢ if predicate then e0 else e1 : τ
impl Infer for Spanned<IfExpr> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        let t0 = self.0.predicate.infer_with(ctx)?;
        t0.try_unify(&MonoType::BOOL, self.1, ctx)?;

        let then = self.0.then.infer_with(ctx)?;
        let or = self.0.or.infer_with(ctx)?;
        let t = then.try_unify(&or, self.1, ctx)?;

        Ok(t)
    }
}

impl Infer for Spanned<CaseExpr> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

// Abstraction rule
// τ = newvar()
// Γ, x : τ ⊢ e : τ'
// ___________________
// Γ ⊢ \x -> e : t -> t'
impl Infer for Spanned<FnExpr> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        let t = MonoType::variable();
        let t_prime = ctx.branch(|ctx| {
            ctx.scopes
                .insert(self.0.param.clone(), PolyType::from(t.clone()));
            self.0.body.infer_with(ctx)
        })?;

        Ok(MonoType::func(t, t_prime))
    }
}

// Application rule
// Γ ⊢ f : τ0
// Γ ⊢ x : τ1
// τ' = newvar()
// unify(τ0, τ1 -> τ')
// --------------------
// Γ ⊢ f x : τ'
impl Infer for Spanned<CallExpr> {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        let t0 = self.0.func.infer_with(ctx)?;
        let t1 = self.0.arg.infer_with(ctx)?;

        let unified = t0.try_unify(&MonoType::func(t1, MonoType::variable()), self.1, ctx)?;

        let t_prime = unified.into_func().unwrap().ret;

        Ok(t_prime)
    }
}

impl Infer for Expr {
    fn infer_with(&self, ctx: &mut Context) -> Result {
        match self {
            Self::Error(_) => todo!(),
            Self::Literal(l) => l.infer_with(ctx),
            Self::Ident(i) => i.infer_with(ctx),
            Self::List(l) => l.infer_with(ctx),
            Self::Record(r) => r.infer_with(ctx),
            Self::RecordSelect(r) => r.infer_with(ctx),
            Self::RecordExtend(r) => r.infer_with(ctx),
            Self::RecordRestrict(r) => r.infer_with(ctx),
            Self::RecordUpdate(r) => r.infer_with(ctx),
            Self::Unary(u) => u.infer_with(ctx),
            Self::Binary(b) => b.infer_with(ctx),
            Self::Let(l) => l.infer_with(ctx),
            Self::If(i) => i.infer_with(ctx),
            Self::Case(c) => c.infer_with(ctx),
            Self::Fn(f) => f.infer_with(ctx),
            Self::Call(c) => c.infer_with(ctx),
        }
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
