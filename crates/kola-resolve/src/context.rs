use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use kola_span::{Report, SourceManager};
use kola_utils::{
    dependency::DependencyGraph, interner::StrInterner, io::FileSystem, visit::VisitMap,
};

use crate::{
    forest::Forest,
    prelude::Topography,
    scope::ModuleScope,
    symbol::{LookupTable, ModuleSym},
};

pub type ModuleGraph = DependencyGraph<ModuleSym>;

pub struct ResolveContext {
    pub report: Report,
    pub io: Box<dyn FileSystem>,
    pub interner: StrInterner,
    pub source_manager: SourceManager,
    pub forest: Forest,
    pub topography: Topography,

    // pub symbol_table: SymbolTable,
    pub lookup_table: LookupTable,
    pub module_graph: ModuleGraph,
    pub unresolved_scopes: HashMap<ModuleSym, ModuleScope>,
    pub module_scopes: HashMap<ModuleSym, Rc<ModuleScope>>,
    pub module_visit: VisitMap<ModuleSym>,
}

impl ResolveContext {
    pub fn new(io: impl FileSystem + 'static) -> Self {
        Self {
            report: Report::new(),
            io: Box::new(io),
            interner: StrInterner::new(),
            source_manager: SourceManager::new(),
            forest: Forest::new(),
            topography: Topography::new(),

            // symbol_table: SymbolTable::new(),
            lookup_table: LookupTable::new(),
            module_graph: ModuleGraph::default(),
            unresolved_scopes: HashMap::new(),
            module_scopes: HashMap::new(),
            module_visit: VisitMap::new(),
        }
    }
}
