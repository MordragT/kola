use std::fmt;

use crate::semantic::{
    error::InferError, merge, Constraints, Kind, Substitutable, Substitution, Unifier, Unify,
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
    fn unify(&self, with: &Self, ctx: &mut Unifier) {
        self.arg.unify(&with.arg, ctx);
        let l = self.ret.apply_cow(&mut ctx.substitution);
        let r = with.ret.apply_cow(&mut ctx.substitution);
        l.unify(&r, ctx);
    }
}

impl Typed for FunctionType {
    fn constrain(&self, with: Kind, _: &mut Constraints) -> Result<(), InferError> {
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
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let arg = self.arg.try_apply(s);
        let ret = self.ret.try_apply(s);

        merge(arg, || self.arg.clone(), ret, || self.ret.clone())
            .map(|(arg, ret)| FunctionType { arg, ret })
    }
}
