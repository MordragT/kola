//! Module for symbol discovery in AST nodes
//!
//! This module contains the visitor implementation for discovering symbols in AST nodes
//! and populating ModuleData with the discovered symbols.

use std::ops::ControlFlow;

use kola_span::{Diagnostic, Loc};
use kola_syntax::prelude::*;
use kola_tree::{
    node::{self, Vis},
    prelude::*,
};

use crate::{
    module::{
        ModuleKey, ValueData, TypeData,
        InlineModuleData, ImportData, ModulePathData,
    },
    manager::ModuleManager,
};

/// Visitor for discovering symbols in a module
pub struct ModuleDiscoverer<'a> {
    /// The current module being processed
    module_key: ModuleKey,

    /// Reference to the module manager
    manager: &'a mut ModuleManager,

    /// Errors accumulated during symbol discovery
    errors: Vec<Diagnostic>,
}

impl<'a> ModuleDiscoverer<'a> {
    /// Create a new symbol discoverer for the given module
    pub fn new(module_key: ModuleKey, manager: &'a mut ModuleManager) -> Self {
        Self {
            module_key,
            manager,
            errors: Vec::new(),
        }
    }

    /// Report a diagnostic error
    fn report(&mut self, diagnostic: impl Into<Diagnostic>) {
        self.errors.push(diagnostic.into());
    }

    /// Get the location of a node in the current module
    fn location<T>(&self, id: Id<T>) -> Option<Loc>
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        let module = self.manager.get_module(self.module_key)?;
        let tree = module.tree.as_ref()?;
        Some(module.spans.get_loc(id, tree)?)
    }

    /// Finish symbol discovery and return any errors
    pub fn finish(self) -> Result<(), Vec<Diagnostic>> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }

    /// Process the current module's tree to discover symbols
    pub fn discover_symbols(&mut self) -> Result<(), Vec<Diagnostic>> {
        let module_key = self.module_key;

        // Get the module's tree
        let tree = match self.manager.get_module(module_key).and_then(|m| m.tree.as_ref()) {
            Some(tree) => tree,
            None => return Ok(()),  // No tree to process
        };

        // Visit the tree to discover symbols
        let module_id = tree.root_id();
        module_id.visit_by(self, tree);

        self.finish()
    }
}

impl<'a, T> Visitor<T> for ModuleDiscoverer<'a>
where
    T: TreeView,
{
    type BreakValue = ();

    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // Get the node data
        let node::ValueBind { vis, name, .. } = *id.get(tree);

        // Extract information
        let vis = *vis.get(tree);
        let name = name.get(tree).0.clone();

        // Get the location
        let location = match self.location(id) {
            Some(loc) => loc,
            None => {
                // If we can't get a location, we can't report meaningful errors
                // so just continue
                return ControlFlow::Continue(());
            }
        };

        // Create the value data
        let value_data = ValueData {
            vis,
            node_id: id,
            location,
        };

        // Add to the module
        if let Some(module) = self.manager.get_module_mut(self.module_key) {
            if let Err(err) = module.insert_value(name, value_data) {
                self.report(err);
            }
        }

        ControlFlow::Continue(())
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // Get the node data
        let node::TypeBind { name, .. } = *id.get(tree);

        // Extract information
        let name = name.get(tree).0.clone();

        // Get the location
        let location = match self.location(id) {
            Some(loc) => loc,
            None => return ControlFlow::Continue(());
        };

        // Create the type data
        let type_data = TypeData {
            vis: Vis::None,  // TypeBind doesn't have visibility in the AST? Adjust if needed
            node_id: id,
            location,
        };

        // Add to the module
        if let Some(module) = self.manager.get_module_mut(self.module_key) {
            if let Err(err) = module.insert_type(name, type_data) {
                self.report(err);
            }
        }

        ControlFlow::Continue(())
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // Get the node data
        let node::ModuleBind { vis, name, value, .. } = *id.get(tree);

        // Extract information
        let vis = *vis.get(tree);
        let name = name.get(tree).0.clone();
        let location = match self.location(id) {
            Some(loc) => loc,
            None => return ControlFlow::Continue(()),
        };

        match *value.get(tree) {
            node::ModuleExpr::Module(module_id) => {
                // Inline module
                let inline_data = InlineModuleData {
                    vis,
                    node_id: module_id,
                    location,
                };

                // First register the module, then process it
                if let Some(module) = self.manager.get_module_mut(self.module_key) {
                    if let Err(err) = module.insert_inline_module(name.clone(), inline_data.clone()) {
                        self.report(err);
                    } else {
                        // Register the inner module with the manager and create the dependency
                        let inner_key = self.manager.register_inline_module(
                            self.module_key,
                            name,
                            inline_data,
                        );

                        // Process the inner module (can be done later in a separate phase)
                        // For now we just record it and move on
                    }
                }
            },
            node::ModuleExpr::Import(import_id) => {
                // Import from file
                let import_path = match tree.get_text(import_id) {
                    Some(path) => path.to_owned(),
                    None => return ControlFlow::Continue(()),
                };

                let path_key = import_path.into();

                let import_data = ImportData {
                    vis,
                    path: path_key,
                    node_id: import_id,
                    location,
                };

                if let Some(module) = self.manager.get_module_mut(self.module_key) {
                    if let Err(err) = module.insert_import(name, import_data) {
                        self.report(err);
                    }
                }
            },
            node::ModuleExpr::Path(path_id) => {
                // Module path reference
                let segments = path_id
                    .get(tree)
                    .0
                    .iter()
                    .map(|id| id.get(tree).0.clone())
                    .collect();

                let path_data = ModulePathData {
                    vis,
                    segments,
                    node_id: path_id,
                    location,
                };

                if let Some(module) = self.manager.get_module_mut(self.module_key) {
                    if let Err(err) = module.insert_module_path(name, path_data) {
                        self.report(err);
                    }
                }
            },
        }

        ControlFlow::Continue(())
    }

    // Don't recurse into nested modules - they will be processed separately
    fn visit_module(&mut self, id: Id<node::Module>, _: &T) -> ControlFlow<Self::BreakValue> {
        // If this is the root module, process it normally
        if id == self.manager.get_module(self.module_key).and_then(|m| m.tree.as_ref().map(|t| t.root_id())).unwrap_or(id) {
            // Continue normal traversal
            ControlFlow::Continue(())
        } else {
            // Skip this module, it will be processed separately
            ControlFlow::Break(())
        }
    }
}
