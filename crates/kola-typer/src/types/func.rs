use kola_protocol::TypeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};
use std::fmt;

use super::{CompType, Kind, MonoType, Typed};
use crate::{
    env::KindEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution, merge},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FuncType {
    pub input: MonoType,
    pub output: CompType,
}

impl FuncType {
    pub fn new(input: MonoType, output: CompType) -> Self {
        Self { input, output }
    }

    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        let Self { input, output } = self;

        let input = input.to_protocol(interner);
        let output = output.to_protocol(interner);

        TypeProtocol::func(input, output)
    }
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.input, self.output)
    }
}

impl Typed for FuncType {
    fn constrain(&self, with: Kind, _env: &mut KindEnv) -> Result<(), TypeError> {
        Err(TypeError::CannotConstrain {
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
