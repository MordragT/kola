//! Module system using phantom-typed module keys and state separation
//!
//! This implementation uses phantom types to create a type-state pattern for
//! module keys, with separate storage for different aspects of module data.

use std::{collections::HashMap, marker::PhantomData};

use kola_span::{Diagnostic, Loc};
use kola_syntax::loc::Locations;
use kola_tree::{
    id::Id,
    node::{self, Vis},
    tree::Tree,
};
use kola_utils::{PathKey, StrKey};

use crate::{dependency::DependencyGraph, error::NameCollision};

//------------------------------------------------------------------------------
// State Marker Traits and Types
//------------------------------------------------------------------------------

/// Trait for module state markers
pub trait ModuleState {}

/// Base state - module is registered but not parsed
pub struct Base;
impl ModuleState for Base {}

/// Parsed state - module has tree and spans
pub struct Parsed;
impl ModuleState for Parsed {}

/// Analyzed state - module has symbols discovered
pub struct Analyzed;
impl ModuleState for Analyzed {}

//------------------------------------------------------------------------------
// Phantom-Typed Module Key
//------------------------------------------------------------------------------

/// Module key with phantom type parameter to track state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypedModuleKey<S: ModuleState> {
    /// The raw module ID
    pub id: u32,

    /// Phantom type to track state
    _state: PhantomData<S>,
}

/// Type aliases for common module key types
pub type BaseModuleKey = TypedModuleKey<Base>;
pub type ParsedModuleKey = TypedModuleKey<Parsed>;
pub type AnalyzedModuleKey = TypedModuleKey<Analyzed>;

impl<S: ModuleState> TypedModuleKey<S> {
    /// Create a new module key with the given ID
    pub fn new(id: u32) -> Self {
        Self {
            id,
            _state: PhantomData,
        }
    }

    /// Get the raw ID
    pub fn raw_id(&self) -> u32 {
        self.id
    }
}

//------------------------------------------------------------------------------
// Data Structures for Different Aspects of Modules
//------------------------------------------------------------------------------

/// Base information about a module (always present)
#[derive(Debug, Clone)]
pub struct BaseData {
    /// Module name (file stem or inline name)
    pub name: StrKey,

    /// How this module is sourced (file or inline)
    pub kind: ModuleKind,

    /// Errors accumulated during processing
    pub errors: Vec<Diagnostic>,
}

/// Data available after parsing
#[derive(Debug, Clone)]
pub struct ParsedData {
    /// The syntax tree
    pub tree: Tree,

    /// Span information
    pub spans: Locations,
}

/// Data available after analysis
#[derive(Debug, Clone)]
pub struct AnalyzedData {
    /// Values discovered during analysis
    pub values: HashMap<StrKey, ValueData>,

    /// Types discovered during analysis
    pub types: HashMap<StrKey, TypeData>,

    /// Inline modules discovered during analysis
    pub inline_modules: HashMap<StrKey, InlineModuleData>,

    /// Imports discovered during analysis
    pub imports: HashMap<StrKey, ImportData>,

    /// Module paths discovered during analysis
    pub module_paths: HashMap<StrKey, ModulePathData>,

    /// Exports discovered during analysis
    pub exports: HashMap<StrKey, ExportKind>,
}

//------------------------------------------------------------------------------
// Module Data Types
//------------------------------------------------------------------------------

/// Source of a module
#[derive(Debug, Clone)]
pub enum ModuleKind {
    /// Module from a file
    File(PathKey),

    /// Inline module defined in another module
    Inline {
        parent_id: u32, // Raw ID, not typed
        span: Loc,
    },
}

/// Information about an import
#[derive(Debug, Clone)]
pub struct ImportData {
    /// Visibility of the import
    pub vis: Vis,

    /// Import path
    pub path: PathKey,

    /// AST node ID
    pub node_id: Id<node::ModuleImport>,

    /// Location in source
    pub location: Loc,
}

/// Information about an inline module
#[derive(Debug, Clone)]
pub struct InlineModuleData {
    /// Visibility of the inline module
    pub vis: Vis,

    /// AST node ID
    pub node_id: Id<node::Module>,

    /// Location in source
    pub location: Loc,
}

/// Information about a module path
#[derive(Debug, Clone)]
pub struct ModulePathData {
    /// Visibility of the module path
    pub vis: Vis,

    /// Path segments
    pub segments: Vec<StrKey>,

    /// AST node ID
    pub node_id: Id<node::ModulePath>,

    /// Location in source
    pub location: Loc,
}

/// Information about a value
#[derive(Debug, Clone)]
pub struct ValueData {
    /// Visibility of the value
    pub vis: Vis,

    /// AST node ID
    pub node_id: Id<node::ValueBind>,

    /// Location in source
    pub location: Loc,
}

/// Information about a type
#[derive(Debug, Clone)]
pub struct TypeData {
    /// Visibility of the type
    pub vis: Vis,

    /// AST node ID
    pub node_id: Id<node::TypeBind>,

    /// Location in source
    pub location: Loc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExportKind {
    /// Exported module
    Module,

    /// Exported value
    Value,

    /// Exported type
    Type,
}

//------------------------------------------------------------------------------
// Module Manager
//------------------------------------------------------------------------------

/// Module manager with separate storage for different module states
pub struct StateSeparatedModuleManager {
    /// Base module data (for all modules)
    base_data: HashMap<u32, BaseData>,

    /// Parsed module data (for parsed and analyzed modules)
    parsed_data: HashMap<u32, ParsedData>,

    /// Analyzed module data (for analyzed modules)
    analyzed_data: HashMap<u32, AnalyzedData>,

    /// Keys for modules in base state
    base_keys: HashMap<u32, BaseModuleKey>,

    /// Keys for modules in parsed state
    parsed_keys: HashMap<u32, ParsedModuleKey>,

    /// Keys for modules in analyzed state
    analyzed_keys: HashMap<u32, AnalyzedModuleKey>,

    /// Next module ID
    next_id: u32,

    /// Dependency graph
    dependencies: DependencyGraph,

    /// Accumulated errors
    errors: Vec<Diagnostic>,
}

/// Error types for the module manager
#[derive(Debug, thiserror::Error)]
pub enum ManagerError {
    /// Module not found
    #[error("Module with ID {0} not found")]
    ModuleNotFound(u32),

    /// Module is in wrong state
    #[error("Module with ID {0} is not in the expected state")]
    WrongState(u32),

    /// Name collision in module
    #[error("Name collision: {0}")]
    NameCollision(#[from] NameCollision),

    /// Diagnostic error
    #[error("{0}")]
    Diagnostic(#[from] Diagnostic),

    /// Multiple diagnostics
    #[error("Multiple errors occurred")]
    MultipleErrors(Vec<Diagnostic>),
}

//------------------------------------------------------------------------------
// Manager Implementation
//------------------------------------------------------------------------------

impl StateSeparatedModuleManager {
    /// Create a new module manager
    pub fn new() -> Self {
        Self {
            base_data: HashMap::new(),
            parsed_data: HashMap::new(),
            analyzed_data: HashMap::new(),
            base_keys: HashMap::new(),
            parsed_keys: HashMap::new(),
            analyzed_keys: HashMap::new(),
            next_id: 0,
            dependencies: DependencyGraph::new(),
            errors: Vec::new(),
        }
    }

    /// Generate a new module ID
    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    //--------------------------------------------------------------------------
    // State checking helpers
    //--------------------------------------------------------------------------

    /// Check if a module exists with the given ID
    fn module_exists(&self, id: u32) -> bool {
        self.base_data.contains_key(&id)
    }

    /// Check if a module is in the base state
    fn is_base_state(&self, id: u32) -> bool {
        self.base_keys.contains_key(&id)
    }

    /// Check if a module is in the parsed state
    fn is_parsed_state(&self, id: u32) -> bool {
        self.parsed_keys.contains_key(&id)
    }

    /// Check if a module is in the analyzed state
    fn is_analyzed_state(&self, id: u32) -> bool {
        self.analyzed_keys.contains_key(&id)
    }

    //--------------------------------------------------------------------------
    // Module registration and state transitions
    //--------------------------------------------------------------------------

    /// Register a file module (creates a module in base state)
    pub fn register_file_module(&mut self, path: PathKey, name: StrKey) -> BaseModuleKey {
        let id = self.next_id();

        // Create base data
        let base = BaseData {
            name,
            kind: ModuleKind::File(path),
            errors: Vec::new(),
        };

        // Create key and store data
        let key = BaseModuleKey::new(id);
        self.base_data.insert(id, base);
        self.base_keys.insert(id, key);

        key
    }

    /// Register an inline module (creates a module in base state)
    pub fn register_inline_module(
        &mut self,
        parent: impl Into<u32>,
        name: StrKey,
        data: InlineModuleData,
    ) -> BaseModuleKey {
        let id = self.next_id();
        let parent_id = parent.into();

        // Create base data
        let base = BaseData {
            name,
            kind: ModuleKind::Inline {
                parent_id,
                span: data.location,
            },
            errors: Vec::new(),
        };

        // Create key and store data
        let key = BaseModuleKey::new(id);
        self.base_data.insert(id, base);
        self.base_keys.insert(id, key);

        // Record dependency
        self.dependencies.add_dependency(parent_id, id);

        key
    }

    /// Parse a module (transition from base to parsed state)
    pub fn parse_module(
        &mut self,
        key: BaseModuleKey,
        tree: Tree,
        spans: Locations,
    ) -> Result<ParsedModuleKey, ManagerError> {
        let id = key.id;

        // Verify module exists and is in base state
        if !self.module_exists(id) {
            return Err(ManagerError::ModuleNotFound(id));
        }

        if !self.is_base_state(id) {
            return Err(ManagerError::WrongState(id));
        }

        // Remove from base keys
        self.base_keys.remove(&id);

        // Create parsed data
        let parsed = ParsedData { tree, spans };

        // Create key and store data
        let parsed_key = ParsedModuleKey::new(id);
        self.parsed_data.insert(id, parsed);
        self.parsed_keys.insert(id, parsed_key);

        Ok(parsed_key)
    }

    /// Analyze a module (transition from parsed to analyzed state)
    pub fn analyze_module(
        &mut self,
        key: ParsedModuleKey,
    ) -> Result<AnalyzedModuleKey, ManagerError> {
        let id = key.id;

        // Verify module exists and is in parsed state
        if !self.module_exists(id) {
            return Err(ManagerError::ModuleNotFound(id));
        }

        if !self.is_parsed_state(id) {
            return Err(ManagerError::WrongState(id));
        }

        // Remove from parsed keys
        self.parsed_keys.remove(&id);

        // Create analyzed data
        let analyzed = AnalyzedData {
            values: HashMap::new(),
            types: HashMap::new(),
            inline_modules: HashMap::new(),
            imports: HashMap::new(),
            module_paths: HashMap::new(),
            exports: HashMap::new(),
        };

        // Create key and store data
        let analyzed_key = AnalyzedModuleKey::new(id);
        self.analyzed_data.insert(id, analyzed);
        self.analyzed_keys.insert(id, analyzed_key);

        Ok(analyzed_key)
    }

    //--------------------------------------------------------------------------
    // Data access methods
    //--------------------------------------------------------------------------

    /// Get base data for any module
    pub fn get_base_data(&self, id: impl Into<u32>) -> Result<&BaseData, ManagerError> {
        let id = id.into();
        self.base_data
            .get(&id)
            .ok_or_else(|| ManagerError::ModuleNotFound(id))
    }

    /// Get mutable base data for any module
    pub fn get_base_data_mut(&mut self, id: impl Into<u32>) -> Result<&mut BaseData, ManagerError> {
        let id = id.into();
        self.base_data
            .get_mut(&id)
            .ok_or_else(|| ManagerError::ModuleNotFound(id))
    }

    /// Get parsed data for a parsed or analyzed module
    pub fn get_parsed_data<S: ModuleState>(
        &self,
        key: TypedModuleKey<S>,
    ) -> Result<&ParsedData, ManagerError> {
        let id = key.id;

        // For parsed modules, verify they're in parsed state
        // For analyzed modules, they also have parsed data
        if !self.is_parsed_state(id) && !self.is_analyzed_state(id) {
            return Err(ManagerError::WrongState(id));
        }

        self.parsed_data
            .get(&id)
            .ok_or_else(|| ManagerError::ModuleNotFound(id))
    }

    /// Get mutable parsed data for a parsed module
    pub fn get_parsed_data_mut(
        &mut self,
        key: ParsedModuleKey,
    ) -> Result<&mut ParsedData, ManagerError> {
        let id = key.id;

        if !self.is_parsed_state(id) {
            return Err(ManagerError::WrongState(id));
        }

        self.parsed_data
            .get_mut(&id)
            .ok_or_else(|| ManagerError::ModuleNotFound(id))
    }

    /// Get analyzed data for an analyzed module
    pub fn get_analyzed_data(&self, key: AnalyzedModuleKey) -> Result<&AnalyzedData, ManagerError> {
        let id = key.id;

        if !self.is_analyzed_state(id) {
            return Err(ManagerError::WrongState(id));
        }

        self.analyzed_data
            .get_mut(&id)
            .ok_or_else(|| ManagerError::ModuleNotFound(id))
    }

    /// Get mutable analyzed data for an analyzed module
    pub fn get_analyzed_data_mut(
        &mut self,
        key: AnalyzedModuleKey,
    ) -> Result<&mut AnalyzedData, ManagerError> {
        let id = key.id;

        if !self.is_analyzed_state(id) {
            return Err(ManagerError::WrongState(id));
        }

        self.analyzed_data
            .get_mut(&id)
            .ok_or_else(|| ManagerError::ModuleNotFound(id))
    }

    //--------------------------------------------------------------------------
    // Combined data access methods
    //--------------------------------------------------------------------------

    /// Get the name of any module
    pub fn get_name(&self, id: impl Into<u32>) -> Result<&StrKey, ManagerError> {
        let id = id.into();
        Ok(&self.get_base_data(id)?.name)
    }

    /// Get the tree for a parsed or analyzed module
    pub fn get_tree<S: ModuleState>(&self, key: TypedModuleKey<S>) -> Result<&Tree, ManagerError> {
        Ok(&self.get_parsed_data(key)?.tree)
    }

    /// Get the span info for a parsed or analyzed module
    pub fn get_spans<S: ModuleState>(
        &self,
        key: TypedModuleKey<S>,
    ) -> Result<&Locations, ManagerError> {
        Ok(&self.get_parsed_data(key)?.spans)
    }

    /// Get a span for a node in a parsed or analyzed module
    pub fn span<S: ModuleState, T>(
        &self,
        key: TypedModuleKey<S>,
        id: Id<T>,
    ) -> Result<Loc, ManagerError>
    where
        T: kola_tree::prelude::MetaCast<kola_syntax::prelude::LocPhase, Meta = Loc>,
    {
        let spans = self.get_spans(key)?;
        Ok(spans.meta(id))
    }

    //--------------------------------------------------------------------------
    // Symbol insertion methods (for analyzed modules)
    //--------------------------------------------------------------------------

    /// Insert a value binding into an analyzed module
    pub fn insert_value(
        &mut self,
        key: AnalyzedModuleKey,
        name: StrKey,
        data: ValueData,
    ) -> Result<(), ManagerError> {
        let analyzed = self.get_analyzed_data_mut(key)?;

        // Check for collisions with other bindings
        if let Some(module) = analyzed.inline_modules.get(&name) {
            return Err(NameCollision::module_bind(
                data.location,
                module.location,
                "Value bindings must have distinct names from module bindings",
            )
            .into());
        }

        // More collision checks omitted for brevity...

        // If the value is exported, add it to exports
        if data.vis == Vis::Export {
            analyzed.exports.insert(name.clone(), ExportKind::Value);
        }

        // Insert the value
        analyzed.values.insert(name, data);

        Ok(())
    }

    // Similar methods for insert_type, insert_inline_module, etc.

    //--------------------------------------------------------------------------
    // Dependency management
    //--------------------------------------------------------------------------

    /// Add a dependency between modules
    pub fn add_dependency(&mut self, from: impl Into<u32>, to: impl Into<u32>) {
        self.dependencies.add_dependency(from.into(), to.into());
    }

    /// Get a topologically sorted list of modules
    pub fn sorted_modules(&self) -> Result<Vec<u32>, crate::dependency::CycleError> {
        self.dependencies.topological_sort()
    }
}

//------------------------------------------------------------------------------
// Helper Traits
//------------------------------------------------------------------------------

/// Trait to convert various module key types to raw IDs
pub trait IntoRawId {
    fn into_raw_id(self) -> u32;
}

impl<S: ModuleState> IntoRawId for TypedModuleKey<S> {
    fn into_raw_id(self) -> u32 {
        self.id
    }
}

impl IntoRawId for u32 {
    fn into_raw_id(self) -> u32 {
        self
    }
}

//------------------------------------------------------------------------------
// Extensions for Analyzed Modules
//------------------------------------------------------------------------------

/// Extension methods for the manager to work with analyzed modules
impl StateSeparatedModuleManager {
    /// Get a value by name from an analyzed module
    pub fn get_value(
        &self,
        key: AnalyzedModuleKey,
        name: &StrKey,
    ) -> Result<Option<&ValueData>, ManagerError> {
        let analyzed = self.get_analyzed_data(key)?;
        Ok(analyzed.values.get(name))
    }

    /// Get a type by name from an analyzed module
    pub fn get_type(
        &self,
        key: AnalyzedModuleKey,
        name: &StrKey,
    ) -> Result<Option<&TypeData>, ManagerError> {
        let analyzed = self.get_analyzed_data(key)?;
        Ok(analyzed.types.get(name))
    }

    /// Check if a name is exported from an analyzed module
    pub fn is_exported(&self, key: AnalyzedModuleKey, name: &StrKey) -> Result<bool, ManagerError> {
        let analyzed = self.get_analyzed_data(key)?;
        Ok(analyzed.exports.contains_key(name))
    }
}
