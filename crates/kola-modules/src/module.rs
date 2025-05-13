//! Module for representing modules and their data.

// *   For functors like `functor (S : SIG) => Body`, `S` acts precisely like a **Module Variable**.
//     It's a name that represents an unknown module *argument* that is required to conform to the signature `SIG`.
// *   When you *apply* this functor to a concrete module, say `MyFunctor(MyModule)`,
//     conceptually, you are indeed "substituting" `S` with `MyModule` inside the `Body`.
//     The most common and elegant way to implement this is not through literal text or AST substitution,
//     but by **extending the environment**.
//     When you process the `Body` of the functor application `MyFunctor(MyModule)`,
//     you perform this processing in an environment where the name `S` is bound to the module `MyModule`.
// *   This environment binding means that any path like `S.x` or `S.T` encountered within the `Body`
//     is resolved by first looking up `S` in the environment (finding `MyModule`)
//     and then looking up `x` or `T` within `MyModule`.
// */
use std::collections::HashMap;

use kola_span::Loc;
use kola_syntax::loc::Locations;
use kola_tree::{
    id::Id,
    node::{self, Vis},
    tree::Tree,
};
use kola_utils::{PathKey, StrKey};

use crate::error::NameCollision;

/// Unique identifier for modules (both file-based and inline)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleKey(pub u32);

/// Data associated with a module
#[derive(Debug)]
pub struct ModuleData {
    pub id: ModuleKey,
    pub name: StrKey,
    pub kind: ModuleKind,
    pub tree: Option<Tree>,
    pub spans: Option<Locations>,
    pub imports: HashMap<StrKey, ImportData>,
    pub inline_modules: HashMap<StrKey, InlineModuleData>,
    pub module_paths: HashMap<StrKey, ModulePathData>,
    pub values: HashMap<StrKey, ValueData>,
    pub types: HashMap<StrKey, TypeData>,
    pub exports: HashMap<StrKey, ExportKind>,
}

/// Macro to check and report a name collision
macro_rules! check_collision {
    ($src_data:expr, $src_kind:expr, $target:expr, $target_kind:expr, $collision_fn:ident) => {
        if let Some(item) = $target {
            return Err(NameCollision::$collision_fn(
                $src_data.location,
                item.location,
                concat!(
                    $src_kind,
                    " bindings must have distinct names from ",
                    $target_kind,
                    " bindings"
                ),
            ));
        }
    };
    ($src_data:expr, $src_kind:expr, $target:expr, $collision_fn:ident) => {
        if let Some(item) = $target {
            return Err(NameCollision::$collision_fn(
                $src_data.location,
                item.location,
                concat!($src_kind, " bindings must have distinct names"),
            ));
        }
    };
}

impl ModuleData {
    /// Creates a new empty module data structure
    pub fn new(id: ModuleKey, name: StrKey, kind: ModuleKind) -> Self {
        Self {
            id,
            name,
            kind,
            tree: None,
            spans: None,
            imports: HashMap::new(),
            inline_modules: HashMap::new(),
            module_paths: HashMap::new(),
            values: HashMap::new(),
            types: HashMap::new(),
            exports: HashMap::new(),
        }
    }

    /// Sets the syntax tree for the module
    pub fn with_tree(mut self, tree: Tree) -> Self {
        self.tree = Some(tree);
        self
    }

    /// Sets the span information for the module
    pub fn with_spans(mut self, spans: Locations) -> Self {
        self.spans = Some(spans);
        self
    }

    /// Insert a value binding, checking for name collisions
    pub fn insert_value(&mut self, name: StrKey, data: ValueData) -> Result<(), NameCollision> {
        // Check for collisions with other bindings
        check_collision!(
            data,
            "Value",
            self.inline_modules.get(&name),
            "module",
            module_bind
        );
        check_collision!(
            data,
            "Value",
            self.imports.get(&name),
            "imported module",
            module_bind
        );
        check_collision!(
            data,
            "Value",
            self.module_paths.get(&name),
            "module path",
            module_bind
        );
        check_collision!(data, "Value", self.types.get(&name), "type", type_bind);
        check_collision!(data, "Value", self.values.get(&name), value_bind);

        // If the value is exported, add it to exports
        if data.vis == Vis::Export {
            self.exports.insert(name, ExportKind::Value);
        }

        // Insert the value
        self.values.insert(name.clone(), data);

        Ok(())
    }

    /// Insert a type binding, checking for name collisions
    pub fn insert_type(&mut self, name: StrKey, data: TypeData) -> Result<(), NameCollision> {
        // Check for collisions with other bindings
        check_collision!(
            data,
            "Type",
            self.inline_modules.get(&name),
            "module",
            module_bind
        );
        check_collision!(
            data,
            "Type",
            self.imports.get(&name),
            "imported module",
            module_bind
        );
        check_collision!(
            data,
            "Type",
            self.module_paths.get(&name),
            "module path",
            module_bind
        );
        check_collision!(data, "Type", self.values.get(&name), "value", value_bind);
        check_collision!(data, "Type", self.types.get(&name), type_bind);

        // If the type is exported, add it to exports
        if data.vis == Vis::Export {
            self.exports.insert(name, ExportKind::Type);
        }

        // Insert the type
        self.types.insert(name.clone(), data);

        Ok(())
    }

    /// Insert an inline module binding, checking for name collisions
    pub fn insert_inline_module(
        &mut self,
        name: StrKey,
        data: InlineModuleData,
    ) -> Result<(), NameCollision> {
        // Check for collisions with other bindings
        check_collision!(
            data,
            "Module",
            self.imports.get(&name),
            "imported module",
            module_bind
        );
        check_collision!(
            data,
            "Module",
            self.module_paths.get(&name),
            "module path",
            module_bind
        );
        check_collision!(data, "Module", self.values.get(&name), "value", value_bind);
        check_collision!(data, "Module", self.types.get(&name), "type", type_bind);
        check_collision!(data, "Module", self.inline_modules.get(&name), module_bind);

        // If the module is exported, add it to exports
        if data.vis == Vis::Export {
            self.exports.insert(name, ExportKind::Module);
        }

        // Insert the module
        self.inline_modules.insert(name.clone(), data);

        Ok(())
    }

    /// Insert an import binding, checking for name collisions
    pub fn insert_import(&mut self, name: StrKey, data: ImportData) -> Result<(), NameCollision> {
        // Check for collisions with other bindings
        check_collision!(
            data,
            "Import",
            self.inline_modules.get(&name),
            "module",
            module_bind
        );
        check_collision!(
            data,
            "Import",
            self.module_paths.get(&name),
            "module path",
            module_bind
        );
        check_collision!(data, "Import", self.values.get(&name), "value", value_bind);
        check_collision!(data, "Import", self.types.get(&name), "type", type_bind);
        check_collision!(data, "Import", self.imports.get(&name), module_bind);

        // If the import is exported, add it to exports
        if data.vis == Vis::Export {
            self.exports.insert(name, ExportKind::Module);
        }

        // Insert the import
        self.imports.insert(name.clone(), data);

        Ok(())
    }

    /// Insert a module path binding, checking for name collisions
    pub fn insert_module_path(
        &mut self,
        name: StrKey,
        data: ModulePathData,
    ) -> Result<(), NameCollision> {
        // Check for collisions with other bindings
        check_collision!(
            data,
            "Module path",
            self.inline_modules.get(&name),
            "module",
            module_bind
        );
        check_collision!(
            data,
            "Module path",
            self.imports.get(&name),
            "import",
            module_bind
        );
        check_collision!(
            data,
            "Module path",
            self.values.get(&name),
            "value",
            value_bind
        );
        check_collision!(
            data,
            "Module path",
            self.types.get(&name),
            "type",
            type_bind
        );
        check_collision!(
            data,
            "Module path",
            self.module_paths.get(&name),
            module_bind
        );

        // If the module path is exported, add it to exports
        if data.vis == Vis::Export {
            self.exports.insert(name, ExportKind::Module);
        }

        // Insert the module path
        self.module_paths.insert(name.clone(), data);

        Ok(())
    }

    /// Get a symbol from the module by name
    pub fn get_bind(&self, name: &StrKey) -> Option<BindData> {
        if let Some(value) = self.values.get(name) {
            return Some(BindData::Value(value));
        }

        if let Some(ty) = self.types.get(name) {
            return Some(BindData::Type(ty));
        }

        if let Some(module) = self.inline_modules.get(name) {
            return Some(BindData::InlineModule(module));
        }

        if let Some(import) = self.imports.get(name) {
            return Some(BindData::Import(import));
        }

        if let Some(path) = self.module_paths.get(name) {
            return Some(BindData::ModulePath(path));
        }

        None
    }

    /// Check if a symbol is exported
    pub fn is_exported(&self, name: &StrKey) -> bool {
        self.exports.contains_key(name)
    }

    /// Get the export kind for a symbol
    pub fn export_kind(&self, name: &StrKey) -> Option<ExportKind> {
        self.exports.get(name).copied()
    }
}

/// Symbol reference in a module
#[derive(Debug, Clone, Copy)]
pub enum BindData<'a> {
    Value(&'a ValueData),
    Type(&'a TypeData),
    InlineModule(&'a InlineModuleData),
    Import(&'a ImportData),
    ModulePath(&'a ModulePathData),
}

impl<'a> BindData<'a> {
    /// Get the visibility of the symbol
    pub fn visibility(&self) -> Vis {
        match self {
            BindData::Value(data) => data.vis,
            BindData::Type(data) => data.vis,
            BindData::InlineModule(data) => data.vis,
            BindData::Import(data) => data.vis,
            BindData::ModulePath(data) => data.vis,
        }
    }

    /// Get the location of the symbol
    pub fn location(&self) -> Loc {
        match self {
            BindData::Value(data) => data.location,
            BindData::Type(data) => data.location,
            BindData::InlineModule(data) => data.location,
            BindData::Import(data) => data.location,
            BindData::ModulePath(data) => data.location,
        }
    }
}

/// Source of a module
#[derive(Debug, Clone)]
pub enum ModuleKind {
    File(PathKey),
    Inline { parent: ModuleKey, span: Loc },
}

/// Information about an import
#[derive(Debug, Clone)]
pub struct ImportData {
    pub vis: Vis,
    pub path: PathKey,
    pub node_id: Id<node::ModuleImport>,
    pub location: Loc,
}

/// Information about an inline module
#[derive(Debug, Clone)]
pub struct InlineModuleData {
    pub vis: Vis,
    pub node_id: Id<node::Module>,
    pub location: Loc,
}

/// Information about a module path
#[derive(Debug, Clone)]
pub struct ModulePathData {
    pub vis: Vis,
    pub segments: Vec<StrKey>,
    pub node_id: Id<node::ModulePath>,
    pub location: Loc,
}

/// Information about a value
#[derive(Debug, Clone)]
pub struct ValueData {
    pub vis: Vis,
    pub node_id: Id<node::ValueBind>,
    pub location: Loc,
}

/// Information about a type
#[derive(Debug, Clone)]
pub struct TypeData {
    pub vis: Vis,
    pub node_id: Id<node::TypeBind>,
    pub location: Loc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExportKind {
    Module,
    Value,
    Type,
}
