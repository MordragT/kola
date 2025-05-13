//! Module for managing the workspace of source files and modules.

use kola_utils::{PathKey, StrKey};

use super::*;

// TODO ModuleMapping ??

/// Manages the workspace of source files and modules in the compiler.
pub struct Workspace {
    /// Source file cache
    pub source_cache: SourceManager,

    /// All modules by ID
    modules: HashMap<ModuleKey, ModuleData>,

    /// File path keys to module IDs
    file_to_module: HashMap<PathKey, ModuleKey>,

    /// Import mappings (module ID, import name) -> imported module ID
    imports: HashMap<(ModuleKey, StrKey), ModuleKey>,

    /// Inline module mappings (parent ID, module name) -> inline module ID
    inline_modules: HashMap<(ModuleKey, StrKey), ModuleKey>,
}

impl Workspace {
    /// Create a new empty workspace
    pub fn new() -> Self {
        Self {
            source_cache: SourceManager::new(),
            modules: HashMap::new(),
            file_to_module: HashMap::new(),
            imports: HashMap::new(),
            inline_modules: HashMap::new(),
        }
    }

    /// Add a module to the workspace
    pub fn add_module(&mut self, id: ModuleKey, data: ModuleData) {
        self.modules.insert(id, data);
    }

    /// Register a file-to-module mapping
    pub fn register_file_module(&mut self, file_key: PathKey, module_id: ModuleKey) {
        self.file_to_module.insert(file_key, module_id);
    }

    /// Find a module by file path
    pub fn find_file_module(&self, path: &Utf8Path) -> Option<ModuleKey> {
        if let Some(key) = self.source_cache.lookup(path) {
            return self.file_to_module.get(&key).copied();
        }
        None
    }

    /// Get module information
    pub fn get_module(&self, id: ModuleKey) -> Option<&ModuleData> {
        self.modules.get(&id)
    }

    /// Get mutable module information
    pub fn get_module_mut(&mut self, id: ModuleKey) -> Option<&mut ModuleData> {
        self.modules.get_mut(&id)
    }

    /// Get all module IDs
    pub fn module_ids(&self) -> impl Iterator<Item = ModuleKey> + '_ {
        self.modules.keys().copied()
    }

    /// Get a module's name
    pub fn module_name(&self, id: ModuleKey) -> String {
        self.modules
            .get(&id)
            .map(|m| m.name.clone())
            .unwrap_or_else(|| format!("<unknown:{}>", id))
    }

    /// Register an import relationship
    pub fn register_import(&mut self, from: ModuleKey, name: StrKey, to: ModuleKey) {
        self.imports.insert((from, name), to);
    }

    /// Register an inline module
    pub fn register_inline_module(&mut self, parent: ModuleKey, name: StrKey, inline: ModuleKey) {
        self.inline_modules.insert((parent, name), inline);
    }

    /// Look up an imported module
    pub fn get_import(&self, from: ModuleKey, name: StrKey) -> Option<ModuleKey> {
        self.imports.get(&(from, name)).copied()
    }

    /// Look up an inline module
    pub fn get_inline_module(&self, parent: ModuleKey, name: StrKey) -> Option<ModuleKey> {
        self.inline_modules.get(&(parent, name)).copied()
    }

    /// Get string from module's string interner
    pub fn get_str(&self, module_id: ModuleKey, key: StrKey) -> Option<&str> {
        self.modules
            .get(&module_id)
            .and_then(|m| m.interner.get(key))
    }
}
