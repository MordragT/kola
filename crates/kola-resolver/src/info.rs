use indexmap::IndexMap;
use kola_span::Loc;
use kola_tree::node::{self, ModuleNamespace};
use kola_utils::dependency::DependencyGraph;

use crate::{
    bind::Bind,
    symbol::{ModuleSym, ValueSym},
};

pub type ModuleGraph = DependencyGraph<ModuleSym>;
pub type ValueGraph = DependencyGraph<ValueSym>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleInfo {
    pub bind: Bind<ModuleNamespace, node::Module>,
    pub loc: Loc,
}

impl ModuleInfo {
    pub fn new(bind: Bind<ModuleNamespace, node::Module>, loc: Loc) -> Self {
        Self { bind, loc }
    }
}

pub type ModuleInfos = IndexMap<ModuleSym, ModuleInfo>;
