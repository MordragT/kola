use serde::{Deserialize, Serialize};
use std::fmt;

use super::{Kind, MonoType, Typed};
use crate::{
    env::KindEnv,
    error::SemanticError,
    substitute::{Substitutable, Substitution, merge},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

impl Typed for FuncType {
    fn constrain(&self, with: Kind, _env: &mut KindEnv) -> Result<(), SemanticError> {
        Err(SemanticError::CannotConstrain {
            expected: with,
            actual: self.clone().into(),
        })
    }
}

impl Substitutable for FuncType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let arg = self.arg.try_apply(s);
        let ret = self.ret.try_apply(s);

        merge(arg, || self.arg.clone(), ret, || self.ret.clone())
            .map(|(arg, ret)| FuncType { arg, ret })
    }
}
