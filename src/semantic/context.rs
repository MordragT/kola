use std::sync::Arc;

use miette::NamedSource;

use crate::syntax::{Source, Span};

use super::{
    error::{InferError, InferReport},
    Cache, Constraints, Scopes, Substitution,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    pub scopes: Scopes,
    pub substitution: Substitution,
    pub cache: Cache,
    pub constraints: Constraints,
    pub errors: Vec<InferError>,
    pub source: Source,
}

impl Context {
    pub fn new(source: Source) -> Self {
        Self {
            scopes: Scopes::new(),
            substitution: Substitution::new(),
            cache: Cache::new(),
            constraints: Constraints::new(),
            errors: Vec::new(),
            source,
        }
    }

    pub fn clear(&mut self) {
        self.scopes.clear();
        self.substitution.clear();
        self.cache.clear();
        self.constraints.clear();
        self.errors.clear();
    }

    pub fn named_source(&self) -> NamedSource<Arc<str>> {
        self.source.named_source()
    }

    pub fn error(&mut self, err: InferError) {
        self.errors.push(err)
    }

    pub fn report_with(&mut self, span: Span) -> InferReport {
        let related = std::mem::take(&mut self.errors);

        InferReport::new(related, span, self.named_source())
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn branch_errors<F, T, E>(&mut self, f: F, context: E) -> T
    where
        F: FnOnce(&mut Self) -> T,
        E: FnOnce(Vec<InferError>) -> InferError,
    {
        let former = std::mem::take(&mut self.errors);
        let result = f(self);
        let errors = std::mem::replace(&mut self.errors, former);
        self.errors.push(context(errors));

        result
    }

    pub fn branch<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.scopes.enter();
        let result = f(self);
        self.scopes.exit();
        result
    }

    // pub fn finish(&mut self, mut ty: MonoType) -> Result<MonoType, InferErrors> {
    //     if self.has_errors() {
    //         Err(self.take_errors())
    //     } else {
    //         let mut s = std::mem::take(&mut self.substitution);
    //         let mut cache = std::mem::take(&mut self.cache);

    //         ty.apply_mut(&mut s, &mut cache);

    //         Ok(ty)
    //     }
    // }
}
