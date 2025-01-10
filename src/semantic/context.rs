use super::{error::InferError, Cache, Constraints, Scopes, Substitution};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
    pub scopes: Scopes,
    pub substitution: Substitution,
    pub cache: Cache,
    pub constrains: Constraints,
    pub errors: Vec<InferError>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.scopes.clear();
        self.substitution.clear();
        self.cache.clear();
        self.constrains.clear();
        self.errors.clear();
    }

    pub fn error(&mut self, err: InferError) {
        self.errors.push(err)
    }

    pub fn take_errors(&mut self) -> Vec<InferError> {
        std::mem::take(&mut self.errors)
    }

    pub fn branch<F, T>(&mut self, mut f: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        self.scopes.enter();
        let result = f(self);
        self.scopes.exit();
        result
    }
}
