use std::fmt;

use derive_more::{Display, From};
use kola_utils::{
    display::DisplayWith,
    interner::{StrInterner, StrKey},
};
use serde::{Deserialize, Serialize};

use crate::{
    class::{CheckClass, TypeClass, TypeClassEnv, TypeClassError},
    kind::{CheckKind, Kind},
    substitute::{Substitutable, Substitution},
    types::TypeVar,
};

use super::Typed;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Label(pub StrKey);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl DisplayWith<StrInterner> for Label {
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
}

impl DisplayWith<StrInterner> for LabelOrVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        match self {
            Self::Var(var) => write!(f, "{var}"),
            Self::Label(key) => key.fmt(f, interner),
        }
    }
}

impl CheckKind for LabelOrVar {
    fn kind(&self) -> Kind {
        Kind::Label
    }
}

impl CheckClass for LabelOrVar {
    fn check_class(&self, with: TypeClass, _env: &mut TypeClassEnv) -> Result<(), TypeClassError> {
        Err(TypeClassError {
            expected: with,
            actual: self.clone().into(),
        })
    }
}

impl Typed for LabelOrVar {}

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
