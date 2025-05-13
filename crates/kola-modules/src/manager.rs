//! Module manager for handling module operations
//!
//! The ModuleManager is responsible for loading, parsing, and managing modules,
//! keeping track of dependencies, and providing access to module information.

use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use kola_span::{Diagnostic, Loc, Report};
use kola_utils::{PathKey, StrKey};

use crate::{
    dependency::DependencyGraph,
    discover::ModuleDiscoverer,
    module::{InlineModuleData, ModuleData, ModuleKey, ModuleKind},
};

/// Manager for handling modules and their dependencies
pub struct ModuleManager {
    modules: HashMap<ModuleKey, ModuleData>,
    next_id: u32,
    dependencies: DependencyGraph,
    report: Report,
}

impl ModuleManager {
    /// Create a new empty module manager
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            next_id: 0,
            dependencies: DependencyGraph::new(),
            report: Report::new(),
        }
    }

    /// Generate a new unique module key
    pub fn new_module_key(&mut self) -> ModuleKey {
        let key = ModuleKey(self.next_id);
        self.next_id += 1;
        key
    }

    /// Get a module by its key
    pub fn get_module(&self, key: ModuleKey) -> Option<&ModuleData> {
        self.modules.get(&key)
    }

    /// Get a mutable reference to a module
    pub fn get_module_mut(&mut self, key: ModuleKey) -> Option<&mut ModuleData> {
        self.modules.get_mut(&key)
    }

    /// Register a module from a file
    pub fn register_file_module(&mut self, path: PathKey, name: StrKey) -> ModuleKey {
        let key = self.new_module_key();

        let module = ModuleData::new(key, name, ModuleKind::File(path));
        self.modules.insert(key, module);

        key
    }

    /// Register an inline module
    pub fn register_inline_module(
        &mut self,
        parent: ModuleKey,
        name: StrKey,
        data: InlineModuleData,
    ) -> ModuleKey {
        let key = self.new_module_key();

        let kind = ModuleKind::Inline {
            parent,
            span: data.location,
        };

        let module = ModuleData::new(key, name, kind);
        self.modules.insert(key, module);

        // Record dependency
        self.dependencies.add_dependency(parent, key);

        key
    }

    /// Add a dependency between modules
    pub fn add_dependency(&mut self, from: ModuleKey, to: ModuleKey) {
        self.dependencies.add_dependency(from, to);
    }

    /// Get a sorted list of modules based on their dependencies
    pub fn sorted_modules(&self) -> Result<Vec<ModuleKey>, crate::dependency::CycleError> {
        self.dependencies.topological_sort()
    }

    /// Parse a file and create a syntax tree for the module
    pub fn parse_module(&mut self, key: ModuleKey, source: &str) -> Result<(), Diagnostic> {
        let module = match self.modules.get_mut(&key) {
            Some(m) => m,
            None => {
                return Err(Diagnostic::error(
                    Loc::new(0, 0),
                    format!("Module not found: {:?}", key),
                ));
            }
        };

        // Here you would use your parser to parse the source code
        // This is a placeholder that would need to be replaced with actual parsing logic
        // Example:
        // let result = FileParser::parse(source);
        // if let Ok((tree, spans)) = result {
        //     module.tree = Some(tree);
        //     module.spans = spans;
        //     Ok(())
        // } else {
        //     Err(result.unwrap_err())
        // }

        // For now, just return Ok for demonstration
        Ok(())
    }

    /// Discover binds in a module using the SymbolDiscoverer visitor
    pub fn discover_binds(&mut self, key: ModuleKey) -> Result<(), Vec<Diagnostic>> {
        let mut discoverer = ModuleDiscoverer::new(key, self);
        discoverer.discover_symbols()
    }

    /// Process all modules in the system
    ///
    /// This method:
    /// 1. Parses all module sources
    /// 2. Discovers symbols in all modules
    /// 3. Checks for dependency cycles
    ///
    /// Returns any accumulated errors
    pub fn process_all(&mut self) -> Result<(), Vec<Diagnostic>> {
        // Get a list of all modules to process
        let module_keys: Vec<ModuleKey> = self.modules.keys().copied().collect();

        // First phase: discover symbols in all modules
        for &key in &module_keys {
            if let Err(mut errs) = self.discover_binds(key) {
                self.errors.append(&mut errs);
            }
        }

        // Second phase: process module paths and imports to establish dependencies
        // This would involve resolving paths, loading imports, etc.
        // Not fully implemented in this example

        // Check for cycles in the dependency graph
        if let Err(cycle_error) = self.dependencies.topological_sort() {
            self.errors.push(Diagnostic::error(
                Loc::new(0, 0),
                format!("Dependency cycle detected: {:?}", cycle_error.path()),
            ));
        }

        // Return errors if any were found
        if self.errors.is_empty() {
            Ok(())
        } else {
            let errors = std::mem::take(&mut self.errors);
            Err(errors)
        }
    }
}

impl Index<ModuleKey> for ModuleManager {
    type Output = ModuleData;

    fn index(&self, index: ModuleKey) -> &Self::Output {
        self.modules.get(&index).unwrap()
    }
}

impl IndexMut<ModuleKey> for ModuleManager {
    fn index_mut(&mut self, index: ModuleKey) -> &mut Self::Output {
        self.modules.get_mut(&index).unwrap()
    }
}
