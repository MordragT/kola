use std::fmt;

use derive_more::{Display, From};
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
    types::TypeVar,
};

use super::{Kind, TypeClass, Typed};

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

impl Substitutable for Label {
    fn try_apply(&self, _s: &mut Substitution) -> Option<Self> {
        None
    }
}

#[derive(
    Debug, Display, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum LabelOrVar {
    /// A label that is a variable.
    Var(TypeVar),
    /// A label that is a string key.
    Label(Label),
}

impl LabelOrVar {
    /// Creates a new `LabelOrVar` variable with a new type variable.
    #[inline]
    pub fn var() -> Self {
        let var = TypeVar::new(Kind::Label);
        Self::Var(var)
    }

    pub fn can_unify(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Var(var), _) | (_, Self::Var(var)) => {
                debug_assert_eq!(var.kind(), Kind::Label);
                true
            }
            (Self::Label(lhs), Self::Label(rhs)) => lhs == rhs,
        }
    }

    pub fn to_protocol(&self, interner: &StrInterner) -> TypeProtocol {
        match self {
            Self::Var(_var) => todo!(),
            Self::Label(label) => label.to_protocol(interner),
        }
    }
}

impl DisplayWithInterner<str> for LabelOrVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        match self {
            Self::Var(var) => write!(f, "{var}"),
            Self::Label(key) => key.fmt(f, interner),
        }
    }
}

impl Typed for LabelOrVar {
    fn kind(&self) -> Kind {
        Kind::Label
    }

    fn constrain_class(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeError> {
        match with {
            _ => Err(TypeError::CannotConstrainClass {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Substitutable for LabelOrVar {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        if let Self::Var(var) = self
            && let Some(mono) = var.try_apply(s)
        {
            let label = mono
                .into_label()
                .expect("Label variables should always resolve to a label");

            Some(label)
        } else {
            None
        }
    }
}
