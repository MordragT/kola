use std::fmt;

use kola_protocol::TypeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};

use crate::{
    env::TypeClassEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution},
};

use super::{Kind, Row, TypeClass, Typed};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VariantType(pub Row);

impl VariantType {
    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        let cases = self.0.to_protocol(interner);
        TypeProtocol::Variant(cases)
    }
}

impl fmt::Display for VariantType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Variant {}", self.0)
    }
}

impl Typed for VariantType {
    fn kind(&self) -> Kind {
        Kind::Type
    }

    fn constrain(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeError> {
        match with {
            TypeClass::Equatable => Ok(()),
            _ => Err(TypeError::CannotConstrain {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Substitutable for VariantType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}
