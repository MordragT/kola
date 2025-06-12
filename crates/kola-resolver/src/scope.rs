use std::rc::Rc;

use derive_more::From;
use indexmap::IndexMap;
use kola_collections::HashMap;
use kola_span::{Loc, SourceId};
use kola_tree::node::{self, ModuleName, ModuleNamespace};

use crate::{GlobalId, bind::Bind, env::LocalEnv, symbol::ModuleSym};

pub type ModuleScopes = IndexMap<ModuleSym, Rc<ModuleScope>>;

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleRef(Vec<ModuleName>);

impl ModuleRef {
    pub fn new(path: Vec<ModuleName>) -> Self {
        Self(path)
    }

    pub fn path(&self) -> &[ModuleName] {
        &self.0
    }
}

#[derive(Debug, Clone)]
pub struct ModuleScope {
    pub env: LocalEnv,
    bind: Bind<ModuleNamespace, node::Module>,
    module_refs: HashMap<ModuleSym, ModuleRef>,
}

impl ModuleScope {
    pub fn new(bind: Bind<ModuleNamespace, node::Module>, loc: Loc) -> Self {
        Self {
            bind,
            env: LocalEnv::new(loc),
            module_refs: HashMap::new(),
        }
    }

    pub fn bind(&self) -> Bind<ModuleNamespace, node::Module> {
        self.bind
    }

    pub fn loc(&self) -> Loc {
        self.env.loc()
    }

    pub fn sym(&self) -> ModuleSym {
        self.bind.sym()
    }

    pub fn global_id(&self) -> GlobalId<node::Module> {
        self.bind.global_id()
    }

    pub fn source(&self) -> SourceId {
        self.bind.source()
    }

    pub fn env(&self) -> &LocalEnv {
        &self.env
    }

    pub fn module_refs(&self) -> &HashMap<ModuleSym, ModuleRef> {
        &self.module_refs
    }

    #[inline]
    pub fn iter_module_refs(&self) -> impl Iterator<Item = (ModuleSym, &ModuleRef)> {
        self.module_refs
            .iter()
            .map(|(&sym, module_ref)| (sym, module_ref))
    }

    pub fn add_module_ref(&mut self, sym: ModuleSym, ref_: impl Into<ModuleRef>) {
        self.module_refs.insert(sym, ref_.into());
    }

    #[inline]
    pub fn get_module_ref(&self, sym: ModuleSym) -> Option<&ModuleRef> {
        self.module_refs.get(&sym)
    }
}
