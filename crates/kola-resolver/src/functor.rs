use crate::{
    scope::ModuleScope,
    symbol::{ModuleSym, Substitute},
};

#[derive(Debug, Clone)]
pub struct Functor {
    pub param: ModuleSym,
    pub body: ModuleScope,
}

impl Functor {
    pub fn new(param: ModuleSym, body: ModuleScope) -> Self {
        Self { param, body }
    }

    // TODO maybe just implement Substitute for ModuleScope ?
    pub fn apply(self, arg: ModuleSym) -> ModuleScope {
        let Self { param, mut body } = self;

        body.shape.substitute_mut(param, arg);
        body.defs.substitute_mut(param, arg);
        body.cons.substitute_mut(param, arg);
        body.resolved.substitute_mut(param, arg);

        body
    }
}
