use super::{Scopes, Substitutions};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
    pub scopes: Scopes,
    pub subs: Substitutions,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }
}
