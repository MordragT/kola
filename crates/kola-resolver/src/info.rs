use indexmap::IndexMap;
use kola_span::{Loc, SourceId};
use kola_tree::{id::Id, node};
use kola_utils::dependency::DependencyGraph;

use crate::symbol::{ModuleSym, ValueSym};

pub type ModuleGraph = DependencyGraph<ModuleSym>;
pub type ValueGraph = DependencyGraph<ValueSym>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleInfo {
    pub id: Id<node::Module>,
    pub sym: ModuleSym,
    pub source: SourceId,
    pub loc: Loc,
}

impl ModuleInfo {
    pub fn new(id: Id<node::Module>, sym: ModuleSym, source: SourceId, loc: Loc) -> Self {
        Self {
            id,
            sym,
            source,
            loc,
        }
    }
}

pub type ModuleInfos = IndexMap<ModuleSym, ModuleInfo>;
