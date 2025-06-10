use serde::{Deserialize, Serialize};
use std::fmt;

use super::{Kind, MonoType, Typed};
use crate::{
    error::SemanticError,
    scope::KindScope,
    substitute::{Substitutable, Substitution, merge},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FuncType {
    pub input: MonoType,
    pub output: MonoType,
}

impl FuncType {
    pub fn new(input: MonoType, output: MonoType) -> Self {
        Self { input, output }
    }
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.input, self.output)
    }
}

impl Typed for FuncType {
    fn constrain(&self, with: Kind, _env: &mut KindScope) -> Result<(), SemanticError> {
        Err(SemanticError::CannotConstrain {
            expected: with,
            actual: self.clone().into(),
        })
    }
}

impl Substitutable for FuncType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let input = self.input.try_apply(s);
        let output = self.output.try_apply(s);

        merge(input, || self.input.clone(), output, || self.output.clone())
            .map(|(input, output)| FuncType { input, output })
    }
}
