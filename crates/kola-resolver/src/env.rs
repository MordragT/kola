use std::collections::HashMap;

use indexmap::IndexMap;
use kola_span::Loc;

use crate::{
    elaborate::ElabJobs,
    lookup::Lookups,
    name::NameMap,
    phase::NodeMap,
    symbol::{AnySym, FunctorSym, ModuleSym, Substitute, merge2},
};

pub type ModuleMap = IndexMap<ModuleSym, Module>;
pub type FunctorMap = IndexMap<FunctorSym, Functor>;

#[derive(Debug, Clone)]
pub struct Module {
    pub loc: Loc,
    pub names: NameMap,
    pub nodes: NodeMap,
}

impl Module {
    pub fn new(loc: Loc) -> Self {
        Self {
            loc,
            names: NameMap::new(),
            nodes: NodeMap::new(),
        }
    }
}

impl Substitute for Module {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let Self { loc, names, nodes } = self;

        let names_opt = names.try_subst(s);
        let nodes_opt = nodes.try_subst(s);

        if let Some((names, nodes)) =
            merge2(names_opt, || names.clone(), nodes_opt, || nodes.clone())
        {
            Some(Self {
                loc: *loc,
                names,
                nodes,
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Functor {
    pub prototype: ModuleSym,
    pub params: Vec<ModuleSym>,
    pub body: Module,
    pub lookups: Lookups,
    pub elab_jobs: ElabJobs,
}

impl Functor {
    pub fn new(
        prototype: ModuleSym,
        params: Vec<ModuleSym>,
        body: Module,
        lookups: Lookups,
        elab_jobs: ElabJobs,
    ) -> Self {
        Self {
            prototype,
            params,
            body,
            elab_jobs,
            lookups,
        }
    }

    pub fn apply(self, instance: ModuleSym, args: Vec<ModuleSym>) -> (Module, Lookups, ElabJobs) {
        let Self {
            prototype,
            params,
            mut body,
            mut elab_jobs,
            mut lookups,
        } = self;

        let mut s = HashMap::new();

        s.insert(AnySym::Module(prototype), AnySym::Module(instance));

        for (param, arg) in params.into_iter().zip(args.into_iter()) {
            s.insert(AnySym::Module(param), AnySym::Module(arg));
        }

        body.subst_mut(&mut s);
        lookups.subst_mut(&mut s);
        elab_jobs.subst_mut(&mut s);

        (body, lookups, elab_jobs)
    }
}
