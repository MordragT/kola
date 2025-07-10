use std::fmt;

use kola_builtins::TypeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};

use crate::{
    env::KindEnv,
    error::TypeError,
    prelude::{Substitutable, Substitution},
    types::{Kind, MonoType, Typed},
};

/// Represents a type representation in the system
/// Useful for Type Reification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TypeRep {
    pub ty: MonoType,
}

impl TypeRep {
    pub fn new(ty: MonoType) -> Self {
        Self { ty }
    }

    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        let ty = self.ty.to_protocol(interner);

        TypeProtocol::type_rep(ty)
    }
}

impl fmt::Display for TypeRep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Type {}", self.ty)
    }
}

impl Typed for TypeRep {
    fn constrain(&self, with: Kind, _env: &mut KindEnv) -> Result<(), TypeError> {
        Err(TypeError::CannotConstrain {
            expected: with,
            actual: self.clone().into(),
        })
    }
}

impl Substitutable for TypeRep {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.ty.try_apply(s).map(Self::new)
    }
}
