//! The `kola-driver` crate coordinates the compilation process for Kola programs.
//!
//! This crate serves as the orchestrator for the entire compilation pipeline, managing:
//! - Source file discovery and loading
//! - Parsing of modules
//! - Tracking dependencies between modules
//! - Coordinating resolution, type checking, and other compilation phases
//!
//! # Example
//! ```no_run
//! use kola_driver::{Driver, DriverOptions};
//! use camino::Utf8Path;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut driver = Driver::new(DriverOptions::default());
//!
//! // Add an entry point file
//! let main_id = driver.add_file(Utf8Path::new("src/main.kl"))?;
//!
//! // Process all dependencies
//! driver.process()?;
//!
//! // Check for errors
//! if driver.has_errors() {
//!     driver.print_diagnostics()?;
//!     std::process::exit(1);
//! }
//!
//! // Get the compiled program for execution
//! let program = driver.get_program()?;
//! # Ok(())
//! # }
//! ```

mod config;
mod dependency;
mod module;
mod workspace;

use camino::{Utf8Path, Utf8PathBuf};
use kola_span::{Diagnostic, Loc, Report, SourceManager};
use kola_syntax::{TokenizeResult, parse_with_source};
use kola_tree::prelude::*;
use std::{
    collections::HashMap,
    io,
    sync::{
        Arc,
        atomic::{AtomicU32, Ordering},
    },
};

pub use config::DriverOptions;
pub use dependency::{CycleError, DependencyGraph};
pub use module::{ModuleData, ModuleKey, ModuleKind};
pub use workspace::Workspace;

/// The main driver for the Kola compiler.
///
/// The `Driver` manages the compilation process, from source discovery
/// to the final stages of compilation. It orchestrates the various phases
/// and maintains the state of the compilation session.
pub struct Driver {
    /// Options for controlling compilation behavior
    pub options: DriverOptions,

    /// The workspace that manages source files
    pub workspace: Workspace,

    /// The dependency graph between modules
    pub dependencies: DependencyGraph,

    /// Compilation diagnostics
    pub report: Report,

    /// Counter for generating unique module IDs
    id_counter: AtomicU32,
}

impl Driver {
    /// Create a new Driver with the given options
    pub fn new(options: DriverOptions) -> Self {
        Self {
            options,
            workspace: Workspace::new(),
            dependencies: DependencyGraph::new(),
            report: Report::new(),
            id_counter: AtomicU32::new(0),
        }
    }

    /// Add a file to be compiled
    pub fn add_file(&mut self, path: &Utf8Path) -> Result<ModuleKey, io::Error> {
        // Check if already loaded
        if let Some(module_id) = self.workspace.find_file_module(path) {
            return Ok(module_id);
        }

        // Load source
        let (path_key, source) = self.workspace.source_cache.get_or_load(path)?;

        // Create a new module ID
        let module_id = self.next_module_id();

        // Parse the file
        self.parse_file(path_key, source.as_ref(), module_id)?;

        Ok(module_id)
    }

    /// Process all loaded modules and their dependencies
    pub fn process(&mut self) -> Result<(), io::Error> {
        // First, discover all dependencies
        self.discover_module_dependencies()?;

        // Check for cycles
        if let Err(cycle_error) = self.dependencies.check_for_cycles() {
            let cycle_path = cycle_error
                .path()
                .iter()
                .map(|&id| self.workspace.module_name(id))
                .collect::<Vec<_>>()
                .join(" -> ");

            self.report.add_issue(cycle_error.into_issue(1001));
        }

        // Sort modules in dependency order
        let ordered_modules = self.dependencies.modules_in_dependency_order();

        // In a complete compiler, you would then:
        // - Perform name resolution
        // - Run type checking
        // - Generate code
        // etc.

        Ok(())
    }

    /// Check if the compilation has encountered any errors
    pub fn has_errors(&self) -> bool {
        self.report.has_errors()
    }

    /// Print all diagnostics
    pub fn print_diagnostics(&self) -> io::Result<()> {
        self.report.print(&self.workspace.source_cache)
    }

    /// Generate a new unique module ID
    fn next_module_id(&self) -> ModuleKey {
        ModuleKey(self.id_counter.fetch_add(1, Ordering::SeqCst))
    }

    /// Parse a file and record its information
    fn parse_file(
        &mut self,
        path_key: PathKey,
        content: &str,
        module_id: ModuleKey,
    ) -> Result<(), io::Error> {
        // Set up parsing
        let file_path = self.workspace.source_cache.interner[path_key].to_owned();

        // Tokenize
        let TokenizeResult { tokens, errors } =
            kola_syntax::tokenize_with_source(content, path_key);

        // Record lexer errors
        for error in errors {
            self.report.add_diagnostic(error.into());
        }

        // Parse if tokenization succeeded
        if let Some(tokens) = tokens {
            // Create EOI location
            let eoi = Loc::new(path_key, kola_span::Span::new(content.len(), content.len()));

            // Set up parse state for extended tracking
            let mut state = kola_syntax::ParseState::new(path_key, module_id);

            // Parse with this state
            let result = parse_with_source(tokens.as_slice(), eoi, &mut state)?;

            // Record parser errors
            for error in result.errors {
                self.report.add_diagnostic(error.into());
            }

            // Create module data
            let module_data = ModuleData {
                id: module_id,
                name: file_path.file_stem().unwrap_or("").to_owned(),
                kind: ModuleKind::File(path_key),
                tree: result.tree,
                interner: Arc::new(state.interner),
                spans: Arc::new(result.spans),

                // Track discovered items
                imports: state.imports,
                inline_modules: state.inline_modules,
                module_paths: state.module_paths,

                // Will be populated during resolution phase
                exports: HashMap::new(),
            };

            // Store module data
            self.workspace.add_module(module_id, module_data);
            self.workspace.register_file_module(path_key, module_id);

            Ok(())
        } else {
            // Tokenization failed completely
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Failed to tokenize file: {}", file_path),
            ))
        }
    }

    /// Discover dependencies for all modules
    fn discover_module_dependencies(&mut self) -> Result<(), io::Error> {
        let modules_to_process = self.workspace.module_ids().collect::<Vec<_>>();

        for &module_id in &modules_to_process {
            // Process imports
            let module_data = self.workspace.get_module(module_id).unwrap();

            for import in &module_data.imports {
                self.process_import(module_id, import)?;
            }

            // Process inline modules
            for inline in &module_data.inline_modules {
                self.process_inline_module(module_id, inline)?;
            }
        }

        Ok(())
    }

    /// Process a single import
    fn process_import(
        &mut self,
        module_id: ModuleKey,
        import: &module::ImportData,
    ) -> Result<(), io::Error> {
        // Resolve import path to a file path
        let import_path = self.resolve_import_path(module_id, &import.path)?;

        // Add the file and parse it
        let imported_id = self.add_file(&import_path)?;

        // Record dependency
        self.dependencies.add_dependency(module_id, imported_id);

        // Register import in workspace
        self.workspace
            .register_import(module_id, import.name, imported_id);

        Ok(())
    }

    /// Process an inline module
    fn process_inline_module(
        &mut self,
        parent_id: ModuleKey,
        inline: &module::InlineModuleData,
    ) -> Result<(), io::Error> {
        // Get the parent module data
        let parent = self.workspace.get_module(parent_id).unwrap();

        // Get the location of the inline module for source extraction
        let loc = parent.spans.get(inline.node_id.into()).inner_copied();

        // Get parent source
        let parent_source_key = match &parent.kind {
            ModuleKind::File(key) => *key,
            ModuleKind::Inline { parent_key, .. } => *parent_key,
        };

        let source = self.workspace.source_cache.get(&parent_source_key).unwrap();

        // Extract the module content
        let content = &source.as_ref()[loc.span().start..loc.span().end];

        // Create a new module ID
        let module_id = self.next_module_id();

        // Create a virtual source for the inline module
        let name = self
            .workspace
            .get_str(parent_id, inline.name)
            .unwrap_or("inline");
        let inline_key = self.workspace.source_cache.create_inline(name, content);

        // Parse the inline module
        let result = self.parse_file(inline_key, content, module_id)?;

        // Update the module source kind to reflect it's an inline module
        if let Some(module_data) = self.workspace.get_module_mut(module_id) {
            module_data.kind = ModuleKind::Inline {
                parent: parent_id,
                parent_key: parent_source_key,
                name: inline.name,
                span: loc,
            };
        }

        // Record dependency
        self.dependencies.add_dependency(parent_id, module_id);

        // Register inline module
        self.workspace
            .register_inline_module(parent_id, inline.name, module_id);

        Ok(())
    }

    /// Resolve an import path to a file path
    fn resolve_import_path(
        &self,
        from_module: ModuleKey,
        import_path: &str,
    ) -> Result<Utf8PathBuf, io::Error> {
        // Get the parent module's file location
        let module = self
            .workspace
            .get_module(from_module)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Module not found"))?;

        // Get the base directory for resolution
        let base_dir = match &module.kind {
            ModuleKind::File(key) => {
                let path = self.workspace.source_cache.interner[*key].to_owned();
                path.parent()
                    .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Invalid parent path"))?
                    .to_owned()
            }
            ModuleKind::Inline { parent_key, .. } => {
                let path = self.workspace.source_cache.interner[*parent_key].to_owned();
                path.parent()
                    .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Invalid parent path"))?
                    .to_owned()
            }
        };

        // Resolve the import path relative to base directory
        let file_path = base_dir.join(import_path).with_extension("kl");

        // Check if path exists
        if !file_path.exists() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("Import not found: {}", file_path),
            ));
        }

        Ok(file_path)
    }
}
