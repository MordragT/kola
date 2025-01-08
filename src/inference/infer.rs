use crate::syntax::ast::*;

use super::{
    error::{InferError, InferResult},
    unify, Context, MonoType, PolyType, TypeConst, TypeVar,
};

pub trait Infer {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType>;
}

// Unit rule
// -----------------------
// Γ ⊢ () : unit
impl Infer for Literal {
    fn infer(&self, _: &mut Context) -> InferResult<MonoType> {
        let ty = match self {
            Self::Bool(_) => TypeConst::Bool,
            Self::Char(_) => TypeConst::Char,
            Self::Num(_) => TypeConst::Num,
            Self::Str(_) => TypeConst::Str,
        };
        Ok(MonoType::Const(ty))
    }
}

// Var rule to infer variable 'x'
// x : σ ∈ Γ   τ = inst(σ)
// -----------------------
// Γ ⊢ x : τ
impl Infer for Ident {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        let t = ctx
            .scopes
            .get(self)
            .ok_or(InferError::Unbound(self.clone()))?
            .instantiate();
        Ok(t)
    }
}

impl Infer for List {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        if let Some((first, rest)) = self.values.split_first() {
            let t0 = first.infer(ctx)?;
            todo!()
        } else {
            todo!()
        }
    }
}

impl Infer for Record {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        todo!()
    }
}

impl Infer for RecordSelect {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        todo!()
    }
}

impl Infer for RecordExtend {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        todo!()
    }
}

impl Infer for RecordRestrict {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        todo!()
    }
}

impl Infer for RecordUpdate {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        todo!()
    }
}

// Abstraction rule
// τ = newvar()
// Γ, x : τ ⊢ e : τ'
// ___________________
// Γ ⊢ \x -> e : t -> t'
impl Infer for UnaryOp {
    fn infer(&self, _: &mut Context) -> InferResult<MonoType> {
        let tc = match self {
            Self::Neg => TypeConst::Num,
            Self::Not => TypeConst::Bool,
        };
        let t = MonoType::Const(tc);

        Ok(MonoType::arrow(t.clone(), t))
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
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        let mut t0 = self.op.infer(ctx)?;
        let t1 = self.target.infer(ctx)?;
        let mut arrow = MonoType::arrow(t1, MonoType::variable());
        unify(&mut t0, &mut arrow, &mut ctx.subs)?;

        let t_prime = arrow.into_arrow().unwrap().1;

        Ok(t_prime)
    }
}

impl Infer for BinaryOp {
    fn infer(&self, _: &mut Context) -> InferResult<MonoType> {
        let t = MonoType::variable();
        let tc = match self {
            Self::Add => TypeConst::Num,
            Self::Sub => TypeConst::Num,
            Self::Mul => TypeConst::Num,
            Self::Div => TypeConst::Num,
            Self::Rem => TypeConst::Num,
            // Comparison
            Self::Less => TypeConst::Bool,
            Self::Greater => TypeConst::Bool,
            Self::LessEq => TypeConst::Bool,
            Self::GreaterEq => TypeConst::Bool,
            // Logical
            Self::And => TypeConst::Bool,
            Self::Or => TypeConst::Bool,
            Self::Xor => TypeConst::Bool,
            // Equality
            Self::Eq => TypeConst::Bool,
            Self::NotEq => TypeConst::Bool,
        };
        let t_prime = MonoType::Const(tc);

        Ok(MonoType::arrow(t.clone(), MonoType::arrow(t, t_prime)))
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
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        let mut t0 = self.op.infer(ctx)?;

        let lhs = self.left.infer(ctx)?;
        let rhs = self.right.infer(ctx)?;

        let mut arrow = MonoType::arrow(lhs, MonoType::arrow(rhs, MonoType::variable()));

        unify(&mut t0, &mut arrow, &mut ctx.subs)?;

        let t_prime = arrow.into_arrow().unwrap().1.into_arrow().unwrap().1;

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
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        TypeVar::enter_level();
        let t = self.value.infer(ctx)?;
        TypeVar::exit_level();

        ctx.scopes.enter();
        // TODO: disallow quanitifaction of variables bound in the context ?
        let bound = ctx.scopes.bound_vars();
        ctx.scopes.insert(self.name.clone(), t.generalize(&bound));
        let t_prime = self.inside.infer(ctx)?;
        ctx.scopes.exit();

        Ok(t_prime)
    }
}

impl Infer for IfExpr {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        todo!()
    }
}

impl Infer for CaseExpr {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        todo!()
    }
}

// Abstraction rule
// τ = newvar()
// Γ, x : τ ⊢ e : τ'
// ___________________
// Γ ⊢ \x -> e : t -> t'
impl Infer for FnExpr {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
        let t = MonoType::variable();

        ctx.scopes.enter();
        ctx.scopes
            .insert(self.param.clone(), PolyType::from(t.clone()));
        let t_prime = self.body.infer(ctx)?;
        ctx.scopes.exit();

        Ok(MonoType::arrow(t, t_prime))
    }
}

impl Infer for Expr {
    fn infer(&self, ctx: &mut Context) -> InferResult<MonoType> {
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
        inference::{error::InferError, Context, Infer, MonoType, TypeConst},
        syntax::{ast::*, Span},
    };

    #[test]
    fn literal() {
        let mut ctx = Context::new();

        let lit = Literal::Num(10.0);
        let ty = lit.infer(&mut ctx).unwrap();

        assert_eq!(MonoType::Const(TypeConst::Num), ty);
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

        assert_eq!(MonoType::Const(TypeConst::Num), ty);

        let unary = UnaryExpr {
            op: UnaryOp::Not,
            target: Box::new(target),
        };

        let err = unary.infer(&mut ctx).unwrap_err();

        assert_eq!(
            InferError::Failure(
                MonoType::Const(TypeConst::Bool),
                MonoType::Const(TypeConst::Num)
            ),
            err
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

        let err = binary.infer(&mut ctx).unwrap_err();

        assert_eq!(
            InferError::Failure(
                MonoType::Const(TypeConst::Bool),
                MonoType::Const(TypeConst::Num)
            ),
            err
        );
    }
}
