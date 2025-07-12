use std::fmt;

use kola_protocol::TypeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};

use super::{Label, MonoType, TypeClass, Typed};
use crate::{
    env::TypeClassEnv,
    error::TypeError,
    prelude::{Substitutable, Substitution},
};

/// Represents a type representation in the system
/// Useful for Type Reification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeWit(pub MonoType);

impl TypeWit {
    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        let ty = self.0.to_protocol(interner);

        TypeProtocol::type_rep(ty)
    }
}

impl fmt::Display for TypeWit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeWit {}", self.0)
    }
}

impl Typed for TypeWit {
    fn constrain(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeError> {
        Err(TypeError::CannotConstrain {
            expected: with,
            actual: self.clone().into(),
        })
    }
}

impl Substitutable for TypeWit {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}

/// Represents a type representation in the system
/// Useful for Type Reification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LabelWit(pub Label);

impl LabelWit {
    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        todo!()
    }
}

impl fmt::Display for LabelWit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LabelWit {}", self.0)
    }
}

impl Typed for LabelWit {
    fn constrain(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeError> {
        Err(TypeError::CannotConstrain {
            expected: with,
            actual: self.clone().into(),
        })
    }
}

impl Substitutable for LabelWit {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}
