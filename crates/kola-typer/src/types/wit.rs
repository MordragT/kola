use std::fmt;

use kola_protocol::TypeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};

use super::{Kind, MonoType, TypeClass, Typed};
use crate::{
    env::TypeClassEnv,
    error::TypeError,
    prelude::{Substitutable, Substitution},
};

/// Represents a type representation in the system
/// Useful for Type Reification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WitType(pub MonoType);

impl WitType {
    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        let ty = self.0.to_protocol(interner);

        ty
    }
}

impl fmt::Display for WitType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeWit {}", self.0)
    }
}

impl Typed for WitType {
    fn kind(&self) -> Kind {
        Kind::Type
    }

    fn constrain_class(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeError> {
        Err(TypeError::CannotConstrainClass {
            expected: with,
            actual: self.clone().into(),
        })
    }
}

impl Substitutable for WitType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}
