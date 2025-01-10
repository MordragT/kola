use crate::syntax::ast::*;

use super::{
    error::{InferError, InferErrors},
    types::{MonoType, PolyType, TypeVar},
    Context,
};

type Result = std::result::Result<MonoType, InferErrors>;

pub trait Infer {
    fn infer(&self, ctx: &mut Context) -> Result;
}

// Unit rule
// -----------------------
// Γ ⊢ () : unit
impl Infer for Literal {
    fn infer(&self, _: &mut Context) -> Result {
        let ty = match self {
            Self::Bool(_) => MonoType::BOOL,
            Self::Char(_) => MonoType::CHAR,
            Self::Num(_) => MonoType::NUM,
            Self::Str(_) => MonoType::STR,
        };
        Ok(ty)
    }
}

// Var rule to infer variable 'x'
// x : σ ∈ Γ   τ = inst(σ)
// -----------------------
// Γ ⊢ x : τ
impl Infer for Ident {
    fn infer(&self, ctx: &mut Context) -> Result {
        let t = ctx
            .scopes
            .get(self)
            .ok_or(InferError::Unbound(self.clone()))?
            .instantiate();
        Ok(t)
    }
}

impl Infer for List {
    fn infer(&self, ctx: &mut Context) -> Result {
        if let Some((first, rest)) = self.values.split_first() {
            let t0 = first.infer(ctx)?;
            todo!()
        } else {
            todo!()
        }
    }
}

impl Infer for Record {
    fn infer(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

// ∀rα. {l :: α | r} → α
impl Infer for RecordSelect {
    fn infer(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

// ∀rα. α → {r} → {l :: α | r}
impl Infer for RecordExtend {
    fn infer(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

// ∀rα. {l :: α | r} → {r}
impl Infer for RecordRestrict {
    fn infer(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

impl Infer for RecordUpdate {
    fn infer(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

// Abstraction rule
// τ = newvar()
// Γ, x : τ ⊢ e : τ'
// ___________________
// Γ ⊢ \x -> e : t -> t'
impl Infer for UnaryOp {
    fn infer(&self, _: &mut Context) -> Result {
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
impl Infer for UnaryExpr {
    fn infer(&self, ctx: &mut Context) -> Result {
        let t0 = self.op.infer(ctx)?;
        let t1 = self.target.infer(ctx)?;

        let unified = t0.try_unify(&MonoType::func(t1, MonoType::variable()), ctx)?;

        let t_prime = unified.into_func().unwrap().ret;

        Ok(t_prime)
    }
}

impl Infer for BinaryOp {
    fn infer(&self, _: &mut Context) -> Result {
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
impl Infer for BinaryExpr {
    fn infer(&self, ctx: &mut Context) -> Result {
        let t0 = self.op.infer(ctx)?;

        let lhs = self.left.infer(ctx)?;
        let rhs = self.right.infer(ctx)?;

        let unified = t0.try_unify(
            &MonoType::func(lhs, MonoType::func(rhs, MonoType::variable())),
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
impl Infer for LetExpr {
    fn infer(&self, ctx: &mut Context) -> Result {
        let t = TypeVar::branch(|| self.value.infer(ctx))?;

        let t_prime = ctx.branch(|ctx| {
            let bound = ctx.scopes.bound_vars();
            ctx.scopes.insert(self.name.clone(), t.generalize(&bound));
            self.inside.infer(ctx)
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
impl Infer for IfExpr {
    fn infer(&self, ctx: &mut Context) -> Result {
        let t0 = self.predicate.infer(ctx)?;
        t0.try_unify(&MonoType::BOOL, ctx)?;

        let then = self.then.infer(ctx)?;
        let or = self.or.infer(ctx)?;
        let t = then.try_unify(&or, ctx)?;

        Ok(t)
    }
}

impl Infer for CaseExpr {
    fn infer(&self, ctx: &mut Context) -> Result {
        todo!()
    }
}

// Abstraction rule
// τ = newvar()
// Γ, x : τ ⊢ e : τ'
// ___________________
// Γ ⊢ \x -> e : t -> t'
impl Infer for FnExpr {
    fn infer(&self, ctx: &mut Context) -> Result {
        let t = MonoType::variable();
        let t_prime = ctx.branch(|ctx| {
            ctx.scopes
                .insert(self.param.clone(), PolyType::from(t.clone()));
            self.body.infer(ctx)
        })?;

        Ok(MonoType::func(t, t_prime))
    }
}

impl Infer for Expr {
    fn infer(&self, ctx: &mut Context) -> Result {
        match self {
            Self::Error(_) => todo!(),
            Self::Literal(l, _) => l.infer(ctx),
            Self::Ident(i, _) => i.infer(ctx),
            Self::List(l, _) => l.infer(ctx),
            Self::Record(r, _) => r.infer(ctx),
            Self::RecordSelect(r, _) => r.infer(ctx),
            Self::RecordExtend(r, _) => r.infer(ctx),
            Self::RecordRestrict(r, _) => r.infer(ctx),
            Self::RecordUpdate(r, _) => r.infer(ctx),
            Self::Unary(u, _) => u.infer(ctx),
            Self::Binary(b, _) => b.infer(ctx),
            Self::Let(l, _) => l.infer(ctx),
            Self::If(i, _) => i.infer(ctx),
            Self::Case(c, _) => c.infer(ctx),
            Self::Fn(f, _) => f.infer(ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        semantic::{error::InferError, types::MonoType, Context, Infer},
        syntax::{ast::*, Span},
    };

    #[test]
    fn literal() {
        let mut ctx = Context::new();

        let lit = Literal::Num(10.0);
        let ty = lit.infer(&mut ctx).unwrap();

        assert_eq!(ty, MonoType::NUM);
    }

    #[test]
    fn unary() {
        let mut ctx = Context::new();

        let span = Span::new(0, 0);
        let target = Expr::Literal(Literal::Num(10.0), span);
        let unary = UnaryExpr {
            op: UnaryOp::Neg,
            target: Box::new(target.clone()),
        };

        let ty = unary.infer(&mut ctx).unwrap();

        assert_eq!(ty, MonoType::NUM);

        let unary = UnaryExpr {
            op: UnaryOp::Not,
            target: Box::new(target),
        };

        let errors = unary.infer(&mut ctx).unwrap_err();

        assert_eq!(
            errors.related[0],
            InferError::CannotUnify {
                expected: MonoType::BOOL,
                actual: MonoType::NUM
            }
        );
    }

    #[test]
    fn binary() {
        let mut ctx = Context::new();

        let lit = |lit| Box::new(Expr::Literal(lit, Span::new(0, 0)));

        let binary = BinaryExpr {
            op: BinaryOp::Eq,
            left: lit(Literal::Bool(true)),
            right: lit(Literal::Num(10.0)),
        };

        let errors = binary.infer(&mut ctx).unwrap_err();

        assert_eq!(
            errors.related[0],
            InferError::CannotUnify {
                expected: MonoType::BOOL,
                actual: MonoType::NUM
            }
        );
    }

    #[test]
    fn let_() {
        let mut ctx = Context::new();

        let span = Span::new(0, 0);

        let let_ = LetExpr {
            name: "x".into(),
            value: Box::new(Expr::Literal(Literal::Num(10.0), span)),
            inside: Box::new(Expr::Ident("x".into(), span)),
        };

        let ty = let_.infer(&mut ctx).unwrap();

        assert_eq!(ty, MonoType::NUM)
    }

    #[test]
    fn if_() {
        let mut ctx = Context::new();

        let lit = |lit| Box::new(Expr::Literal(lit, Span::new(0, 0)));

        let if_ = IfExpr {
            predicate: lit(Literal::Bool(true)),
            then: lit(Literal::Num(5.0)),
            or: lit(Literal::Num(10.0)),
        };

        let ty = if_.infer(&mut ctx).unwrap();

        assert_eq!(ty, MonoType::NUM);
        ctx.clear();

        let if_ = IfExpr {
            predicate: lit(Literal::Bool(true)),
            then: lit(Literal::Num(5.0)),
            or: lit(Literal::Char('x')),
        };

        let errors = if_.infer(&mut ctx).unwrap_err();

        assert_eq!(
            errors.related[0],
            InferError::CannotUnify {
                expected: MonoType::NUM,
                actual: MonoType::CHAR,
            }
        );
    }
}
