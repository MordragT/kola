use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use kola_span::{Report, SourceManager};
use kola_utils::{
    dependency::DependencyGraph,
    interner::{StrInterner, StrKey},
    io::FileSystem,
};

use crate::{
    forest::Forest,
    prelude::Topography,
    scope::{LexicalScope, ModuleScope},
    symbol::{DefTable, ModuleSymbol, SymbolTable},
};

pub type ModuleGraph = DependencyGraph<ModuleSymbol>;

pub struct ResolveContext {
    pub report: Report,
    pub io: Box<dyn FileSystem>,
    pub interner: StrInterner,
    pub source_manager: SourceManager,
    pub forest: Forest,
    pub topography: Topography,

    pub symbol_table: SymbolTable,
    // pub def_table: DefTable,
    pub module_graph: ModuleGraph,
    pub module_scopes: HashMap<ModuleSymbol, Rc<ModuleScope>>,
    // pub module_paths: HashMap<ModuleSymbol, Vec<StrKey>>,
    pub pending: HashSet<ModuleSymbol>,
    pub active: HashSet<ModuleSymbol>,
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

            symbol_table: SymbolTable::new(),
            // def_table: DefTable::new(),
            module_graph: ModuleGraph::default(),
            module_scopes: HashMap::new(),
            // module_paths: HashMap::new(),
            pending: HashSet::new(),
            active: HashSet::new(),
        }
    }
}
