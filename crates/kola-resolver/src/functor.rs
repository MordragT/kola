use std::collections::HashMap;

use kola_tree::meta::MetaMapExt;

use crate::{
    scope::ModuleScope,
    symbol::{AnySym, ModuleSym, Substitute},
};

#[derive(Debug, Clone)]
pub struct Functor {
    pub params: Vec<ModuleSym>,
    pub body: ModuleScope,
}

impl Functor {
    pub fn new(params: Vec<ModuleSym>, body: ModuleScope) -> Self {
        Self { params, body }
    }

    // TODO maybe just implement Substitute for ModuleScope ?
    pub fn apply(self, bind: ModuleSym, args: Vec<ModuleSym>) -> ModuleScope {
        let Self { params, mut body } = self;

        let old_bind = std::mem::replace(&mut body.info.sym, bind);
        let id = body.info.id;
        body.resolved.insert_meta(id, bind);

        let mut subst = params
            .into_iter()
            .map(AnySym::Module)
            .zip(args.into_iter().map(AnySym::Module))
            .collect::<HashMap<_, _>>();

        dbg!(old_bind, bind);

        subst.insert(AnySym::Module(old_bind), AnySym::Module(bind));

        body.shape.subst_mut(&subst);
        body.defs.subst_mut(&subst);
        body.cons.subst_mut(&subst);
        body.resolved.subst_mut(&subst);

        body
    }
}
