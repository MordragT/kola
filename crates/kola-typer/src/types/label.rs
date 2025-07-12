use std::fmt;

use kola_protocol::TypeProtocol;
use kola_utils::{
    interner::{StrInterner, StrKey},
    interner_ext::DisplayWithInterner,
};
use serde::{Deserialize, Serialize};

use crate::{
    env::TypeClassEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution},
};

use super::{TypeClass, Typed};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Label(pub StrKey);

impl Label {
    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        TypeProtocol::label(interner[self.0].to_owned())
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl DisplayWithInterner<str> for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        write!(f, "{}", interner[self.0])
    }
}

impl Typed for Label {
    fn constrain(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeError> {
        match with {
            _ => Err(TypeError::CannotConstrain {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Substitutable for Label {
    fn try_apply(&self, _s: &mut Substitution) -> Option<Self> {
        None
    }
}
