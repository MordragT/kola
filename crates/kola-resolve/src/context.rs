use std::collections::{HashMap, HashSet};

use kola_span::{Report, SourceManager};
use kola_utils::{dependency::DependencyGraph, interner::StrInterner, io::FileSystem};

use crate::{
    forest::Forest,
    module::{ModuleKey, ModuleKeyMapping, ModuleScopes, UnresolvedModuleScope},
    prelude::Topography,
};

pub struct ResolveContext {
    pub report: Report,
    pub io: Box<dyn FileSystem>,
    pub interner: StrInterner,
    pub source_manager: SourceManager,
    pub forest: Forest,
    pub topography: Topography,
    pub mapping: ModuleKeyMapping,
    pub dependencies: DependencyGraph<ModuleKey>,
    pub scopes: ModuleScopes,
    pub unresolved: HashMap<ModuleKey, UnresolvedModuleScope>,
    pub in_progress: HashSet<ModuleKey>,
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
            mapping: ModuleKeyMapping::default(),
            dependencies: DependencyGraph::default(),
            scopes: ModuleScopes::default(),
            unresolved: HashMap::new(),
            in_progress: HashSet::new(),
        }
    }
}
