use std::fmt;

use serde::{Deserialize, Serialize};

use crate::{
    env::KindEnv,
    error::TypeError,
    substitute::{Substitutable, Substitution},
};

use super::{Kind, MonoType, Typed};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ListType {
    pub el: MonoType,
}

impl ListType {
    pub fn new(el: MonoType) -> Self {
        Self { el }
    }
}

impl fmt::Display for ListType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "List {}", self.el)
    }
}

impl Typed for ListType {
    fn constrain(&self, with: Kind, _env: &mut KindEnv) -> Result<(), TypeError> {
        match with {
            Kind::Addable | Kind::Comparable | Kind::Equatable => Ok(()),
            _ => Err(TypeError::CannotConstrain {
                expected: with,
                actual: self.clone().into(),
            }),
        }
    }
}

impl Substitutable for ListType {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        self.el.try_apply(s).map(Self::new)
    }
}
