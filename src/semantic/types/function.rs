use std::fmt;

use crate::semantic::{
    error::{InferError, InferResult},
    merge, Cache, Constraints, Context, Kind, Substitutable, Substitution, Unify,
};

use super::{MonoType, TypeVar, Typed};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub arg: MonoType,
    pub ret: MonoType,
}

impl FunctionType {
    pub fn new(arg: MonoType, ret: MonoType) -> Self {
        Self { arg, ret }
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.arg, self.ret)
    }
}

impl Unify<&Self> for FunctionType {
    fn unify(&self, with: &Self, ctx: &mut Context) {
        self.arg.unify(&with.arg, ctx);
        let l = self.ret.apply_cow(&mut ctx.substitution, &mut ctx.cache);
        let r = with.ret.apply_cow(&mut ctx.substitution, &mut ctx.cache);
        l.unify(&r, ctx);
    }
}

impl Typed for FunctionType {
    fn constrain(&self, with: Kind, _: &mut Constraints) -> InferResult<()> {
        Err(InferError::CannotConstrain {
            expected: with,
            actual: self.clone().into(),
        })
    }

    fn contains(&self, tv: TypeVar) -> bool {
        self.arg.contains(tv) || self.ret.contains(tv)
    }

    fn type_vars(&self, vars: &mut Vec<TypeVar>) {
        self.arg.type_vars(vars);
        self.ret.type_vars(vars);
    }
}

impl Substitutable for FunctionType {
    fn try_apply(&self, s: &mut Substitution, cache: &mut Cache) -> Option<Self> {
        let arg = self.arg.try_apply(s, cache);
        let ret = self.ret.try_apply(s, cache);

        merge(arg, || self.arg.clone(), ret, || self.ret.clone())
            .map(|(arg, ret)| FunctionType { arg, ret })
    }
}
