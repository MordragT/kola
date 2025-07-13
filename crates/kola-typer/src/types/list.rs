use std::fmt;

use kola_protocol::TypeProtocol;
use kola_utils::interner::StrInterner;
use serde::{Deserialize, Serialize};

use crate::{
    env::TypeClassEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution},
};

use super::{Kind, MonoType, TypeClass, Typed};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ListType(pub MonoType);

impl ListType {
    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        let el = self.0.to_protocol(interner);

        TypeProtocol::list(el)
    }
}

impl fmt::Display for ListType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "List {}", self.0)
    }
}

impl Typed for ListType {
    fn kind(&self) -> Kind {
        Kind::Type
    }

    fn constrain_class(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeError> {
        match with {
            TypeClass::Addable | TypeClass::Comparable | TypeClass::Equatable => Ok(()),
            _ => Err(TypeError::CannotConstrainClass {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Substitutable for ListType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}
