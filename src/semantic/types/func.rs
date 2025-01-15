use std::fmt;

use crate::semantic::{merge, Substitutable, Substitution};

use super::{MonoType, Typed};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncType {
    pub arg: MonoType,
    pub ret: MonoType,
}

impl FuncType {
    pub fn new(arg: MonoType, ret: MonoType) -> Self {
        Self { arg, ret }
    }
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.arg, self.ret)
    }
}

impl Typed for FuncType {}

impl Substitutable for FuncType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let arg = self.arg.try_apply(s);
        let ret = self.ret.try_apply(s);

        merge(arg, || self.arg.clone(), ret, || self.ret.clone())
            .map(|(arg, ret)| FuncType { arg, ret })
    }
}
