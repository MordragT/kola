use std::{collections::HashMap, ops::ControlFlow};

use indexmap::IndexMap;
use kola_builtins::{BuiltinType, find_builtin_id};
use kola_collections::scope::LinearScope;
use kola_interner::StrInterner;
use kola_span::{Diagnostic, Loc, Report, SourceId};
use kola_tree::{
    node::{ModuleName, ModuleTypeName, TypeName, ValueName},
    prelude::*,
};

use crate::{
    def::{AnyDef, DefMap},
    elaborate::{ElabJob, ElabJobs},
    env::{Module, ModuleMap},
    error::name_collision,
    lookup::{
        Lookups, ModuleLookup, ModuleTypeAnnotLookup, ModuleTypeLookup, TypeAnnotLookup,
        TypeLookup, ValueLookup,
    },
    phase::{NodeMap, ResolvedModuleType, ResolvedType, ResolvedValue},
    symbol::{
        FileMap, ModuleGraph, ModuleSym, ModuleTypeGraph, ModuleTypeSym, TypeGraph, TypeSym,
        ValueGraph, ValueSym,
    },
};

#[derive(Debug, Clone)]
pub struct DiscoverOutput {
    pub report: Report,
}

pub fn discover(
    sym: ModuleSym,
    source: SourceId,
    key: ModuleKey,
    tree: &Tree,
    files: &FileMap,
    module_of: &HashMap<(SourceId, ModuleKey), ModuleSym>,
    interner: &StrInterner,
    modules: &mut ModuleMap,
    entry_points: &mut Vec<ValueSym>,
    defs: &mut DefMap,
    value_graph_map: &mut IndexMap<ModuleSym, ValueGraph>,
    type_graph_map: &mut IndexMap<ModuleSym, TypeGraph>,
    module_type_graph_map: &mut IndexMap<ModuleSym, ModuleTypeGraph>,
    module_graph: &mut ModuleGraph,
    lookups: &mut Lookups,
    elab_jobs: &mut ElabJobs,
) -> DiscoverOutput {
    let loc = *tree.get_span(tree.root_id());

    value_graph_map.insert(sym, ValueGraph::new());
    type_graph_map.insert(sym, TypeGraph::new());
    module_type_graph_map.insert(sym, ModuleTypeGraph::new());
    module_graph.add_node(sym);

    // Create a visitor to walk the tree and collect declarations
    let mut discoverer = Discoverer::new(
        sym,
        source,
        key,
        loc,
        tree,
        files,
        module_of,
        interner,
        modules,
        entry_points,
        defs,
        value_graph_map,
        type_graph_map,
        module_type_graph_map,
        module_graph,
        lookups,
        elab_jobs,
    );

    ControlFlow::Continue(()) = tree.root_id().visit_by(&mut discoverer, tree);

    DiscoverOutput {
        report: discoverer.report,
    }
}

pub type ValueScope = LinearScope<ValueName, ValueSym>;
pub type TypeScope = LinearScope<TypeName, TypeSym>;

#[derive(Debug, Clone, Default)]
struct Scopes {
    value: ValueScope,
    type_: TypeScope,
}

#[derive(Debug, Clone, Copy, Default)]
struct Bindings {
    module: Option<ModuleSym>,
    module_type: Option<ModuleTypeSym>,
    type_: Option<TypeSym>,
    value: Option<ValueSym>,
}

impl Bindings {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn replace(&mut self) -> Self {
        std::mem::take(self)
    }
}

struct Discoverer<'a> {
    root: ModuleSym,
    root_key: ModuleKey,
    source: SourceId,
    scope: Module,
    lookups: &'a mut Lookups,
    elab_jobs: &'a mut ElabJobs,
    bindings: Bindings,
    scopes: Scopes,
    report: Report,
    locs: &'a Tree,
    files: &'a FileMap,
    module_of: &'a HashMap<(SourceId, ModuleKey), ModuleSym>,
    interner: &'a StrInterner,
    modules: &'a mut ModuleMap,
    entry_points: &'a mut Vec<ValueSym>,
    defs: &'a mut DefMap,
    value_graph_map: &'a mut IndexMap<ModuleSym, ValueGraph>,
    type_graph_map: &'a mut IndexMap<ModuleSym, TypeGraph>,
    module_type_graph_map: &'a mut IndexMap<ModuleSym, ModuleTypeGraph>,
    module_graph: &'a mut ModuleGraph,
}

impl<'a> Discoverer<'a> {
    fn new(
        root: ModuleSym,
        source: SourceId,
        root_key: ModuleKey,
        loc: Loc,
        locs: &'a Tree,
        files: &'a FileMap,
        module_of: &'a HashMap<(SourceId, ModuleKey), ModuleSym>,
        interner: &'a StrInterner,
        modules: &'a mut ModuleMap,
        entry_points: &'a mut Vec<ValueSym>,
        defs: &'a mut DefMap,
        value_graph_map: &'a mut IndexMap<ModuleSym, ValueGraph>,
        type_graph_map: &'a mut IndexMap<ModuleSym, TypeGraph>,
        module_type_graph_map: &'a mut IndexMap<ModuleSym, ModuleTypeGraph>,
        module_graph: &'a mut ModuleGraph,
        lookups: &'a mut Lookups,
        elab_jobs: &'a mut ElabJobs,
    ) -> Self {
        Self {
            root,
            root_key,
            source,
            scope: Module::new(loc, root_key),
            lookups,
            elab_jobs,
            bindings: Bindings::new(),
            scopes: Scopes::default(),
            report: Report::new(),
            locs,
            files,
            module_of,
            interner,
            modules,
            entry_points,
            defs,
            value_graph_map,
            type_graph_map,
            module_type_graph_map,
            module_graph,
        }
    }

    fn name_collision(&mut self, a: impl Into<AnyDef>, b: impl Into<AnyDef>) {
        let a = a.into();
        let b = b.into();
        let error = name_collision((a.kind(), a.loc()), (b.kind(), b.loc()));
        self.report.add_diagnostic(error.into());
    }

    #[inline]
    fn span<T: 'static>(&self, id: Id<T>) -> Loc
    where
        Tree: TotalSpan<T, Loc>,
    {
        *self.locs.get_span(id)
    }

    fn insert_symbol<T: 'static, S: 'static>(&mut self, id: Id<T>, sym: S)
    where
        NodeMap: PartialRelationSet<T, S>,
    {
        self.scope.nodes.set(id, sym);
    }

    pub fn insert_module_type(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        loc: Loc,
        vis: node::Vis,
        name: ModuleTypeName,
        sym: ModuleTypeSym,
    ) {
        if let Some(bind) = self
            .scope
            .names
            .get(name)
            .and_then(|binding| self.defs.get(binding.sym))
        {
            self.name_collision((id, loc), bind);
            return;
        }

        self.scope.names.insert_module_type(name, vis, sym);
        self.defs.insert_module_type(sym, id, loc);
    }

    pub fn insert_module(
        &mut self,
        id: Id<node::ModuleBind>,
        loc: Loc,
        vis: node::Vis,
        name: ModuleName,
        sym: ModuleSym,
    ) {
        if let Some(bind) = self
            .scope
            .names
            .get(name)
            .and_then(|binding| self.defs.get(binding.sym))
        {
            self.name_collision((id, loc), bind);
            return;
        }

        self.scope.names.insert_module(name, vis, sym);
        self.defs.insert_module(sym, id, loc);
    }

    pub fn insert_type(
        &mut self,
        id: Id<node::TypeBind>,
        loc: Loc,
        vis: node::Vis,
        name: TypeName,
        sym: TypeSym,
    ) {
        if let Some(bind) = self
            .scope
            .names
            .get(name)
            .and_then(|binding| self.defs.get(binding.sym))
        {
            self.name_collision((id, loc), bind);
            return;
        }

        self.scope.names.insert_type(name, vis, sym);
        self.defs.insert_type(sym, id, loc);
    }

    pub fn insert_value(
        &mut self,
        id: Id<node::ValueBind>,
        loc: Loc,
        vis: node::Vis,
        name: ValueName,
        sym: ValueSym,
    ) {
        if let Some(bind) = self
            .scope
            .names
            .get(name)
            .and_then(|binding| self.defs.get(binding.sym))
        {
            self.name_collision((id, loc), bind);
            return;
        }

        self.scope.names.insert_value(name, vis, sym);
        self.defs.insert_value(sym, id, loc);
    }

    fn with_fresh_scope<T, F>(&mut self, child: ModuleSym, loc: Loc, f: F) -> (T, Module)
    where
        F: FnOnce(&mut Self) -> T,
    {
        let parent = self.root;

        // Skip adding dependencies if child is the same as parent,
        // which should only happen in the root module
        if child != parent {
            self.value_graph_map.insert(child, ValueGraph::new());
            self.type_graph_map.insert(child, TypeGraph::new());
            self.module_type_graph_map
                .insert(child, ModuleTypeGraph::new());
        }

        let saved = self.bindings.replace();

        // One discoverer handles exactly one module tree, so the fresh scope is
        // always the root module of this tree.
        let mut child_scope = Module::new(loc, self.root_key);
        std::mem::swap(&mut child_scope, &mut self.scope);
        self.root = child;

        let result = f(self);

        std::mem::swap(&mut child_scope, &mut self.scope);
        self.root = parent;
        self.bindings = saved;

        (result, child_scope)
    }
}

impl<'a> Visitor<Loc> for Discoverer<'a> {
    type BreakValue = !;

    fn visit_module_type_bind(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleTypeBind { vis, name, .. } = *tree.get(id);

        let vis = *tree.get(vis);
        let name = *tree.get(name);

        let loc = self.span(id);

        let module_type_sym = ModuleTypeSym::new();
        self.insert_symbol(id, module_type_sym);
        self.bindings.module_type = Some(module_type_sym);

        // Register the module type binding in the current scope
        self.insert_module_type(id, loc, vis, name, module_type_sym);
        self.walk_module_type_bind(id, tree)?;

        self.bindings.module_type = None;

        ControlFlow::Continue(())
    }

    fn visit_qualified_module_type(
        &mut self,
        id: Id<node::QualifiedModuleType>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedModuleType { path, ty } = *tree.get(id);

        let name = *ty.get(tree);
        let loc = self.span(id);

        if let Some(path) = path {
            // Module Type defined in another module - Just visit the module path if it exists
            self.visit_module_path(path, tree)?;
        } else if let Some(binding) = self.scope.names.get_module_type(name) {
            // Found a module type binding in the current module scope
            // which was defined before this reference to it (no forward reference)

            self.insert_symbol(id, ResolvedModuleType(binding.sym));

            // Qualified Module Types can occur in both module type binds and module type annotations.
            // Only in the former case we need to add a dependency.
            if let Some(current_sym) = self.bindings.module_type {
                // Add dependency from the current type bind to this type
                self.module_type_graph_map[&self.root].add_dependency(current_sym, binding.sym);
            }
        }
        // Either a forward reference or not found in the current module scope
        else if let Some(current_sym) = self.bindings.module_type {
            let lookup = ModuleTypeLookup::new(name, id, current_sym, loc, self.root);
            self.lookups.insert_module_type(lookup);
        } else {
            let lookup = ModuleTypeAnnotLookup::new(name, id, loc, self.root);
            self.lookups.insert_module_type_annot(lookup);
        }
        ControlFlow::Continue(())
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleBind {
            vis, name, value, ..
        } = *tree.get(id);

        let name = *tree.get(name);
        let vis = tree.get(vis);
        let loc = self.span(id);

        let Some(value) = value else {
            // TODO(phase B): record abstract module bindings as obligations.
            self.report.add_diagnostic(Diagnostic::error(
                loc,
                "Abstract module bindings are not implemented yet",
            ));
            return ControlFlow::Continue(());
        };

        match *tree.get(value) {
            node::ModuleExpr::Import(import_id) => {
                // If this is an import of another file, we can skip creating a new module and just reference the imported module's symbol
                let target = import_id.get(tree).0;
                let sym = self.files[&target];

                self.insert_symbol(id, sym);
                self.insert_symbol(import_id, sym);
                self.insert_module(id, loc, *vis, name, sym);
                self.module_graph.add_dependency(self.root, sym);

                ControlFlow::Continue(())
            }
            node::ModuleExpr::Path(path_id) => {
                let path = tree
                    .get(path_id)
                    .0
                    .get(tree)
                    .iter()
                    .map(|id| *id.get(tree))
                    .collect();

                let job = ElabJob::path(id, loc, *vis, name, self.root, path_id, loc, path);
                self.elab_jobs.push_back(job);

                ControlFlow::Continue(())
            }
            node::ModuleExpr::Body(module_ref) => {
                // The body lives in its own tree; this bind only references it.
                let key = module_ref.get(tree).0;
                let sym = self.module_of[&(self.source, key)];

                self.insert_symbol(id, sym);
                self.insert_module(id, loc, *vis, name, sym);
                self.module_graph.add_dependency(self.root, sym);

                // Still visit the module type annotation, if any.
                self.walk_module_bind(id, tree)
            }
            node::ModuleExpr::Merge(_) => {
                // TODO(phase C): compose module surfaces for `with`/`over`/`using`.
                self.report.add_diagnostic(Diagnostic::error(
                    loc,
                    "Module composition is not implemented yet",
                ));
                ControlFlow::Continue(())
            }
            node::ModuleExpr::Error(_) => ControlFlow::Continue(()),
        }
    }

    fn visit_module_body(
        &mut self,
        id: Id<node::ModuleBody>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        // Either inline module or root module
        let sym = self.bindings.module.take().unwrap_or(self.root);
        let loc = self.span(id);

        if sym != self.root {
            // Only add dependency if this is not the root module
            self.module_graph.add_dependency(self.root, sym);
        }

        let (_, mut module) = self.with_fresh_scope(sym, loc, |this| {
            let _ = this.walk_module_body(id, tree);
        });

        module.nodes.set(id, sym);
        self.modules.insert(sym, module);

        ControlFlow::Continue(())
    }

    fn visit_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let path = tree
            .get(id)
            .0
            .get(tree)
            .iter()
            .map(|id| *id.get(tree))
            .collect::<Vec<_>>();

        let loc = self.span(id);

        // This visit method will only be called from module path's in value position
        self.lookups
            .insert_module(ModuleLookup::new(path, id, self.root, loc));

        ControlFlow::Continue(())
    }

    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueBind {
            vis,
            name,
            ty_scheme,
            value,
        } = *tree.get(id);

        let name = *tree.get(name);
        let vis = tree.get(vis);

        let loc = self.span(id);

        let value_sym = ValueSym::new();
        self.insert_symbol(id, value_sym);
        self.bindings.value = Some(value_sym);
        self.value_graph_map[&self.root].add_node(value_sym);

        if self.interner.get(name.0) == Some("main") {
            // If this is the main entry point, we will collect it later
            self.entry_points.push(value_sym);
        }

        // Register the value binding in the current scope
        self.insert_value(id, loc, *vis, name, value_sym);

        // Intuitively I would guess that this is always 0
        // and I could just create a fresh type scope for every type and value bind.
        let depth = self.scopes.type_.depth();

        if let Some(ty_scheme) = ty_scheme {
            // If there is a type annotation, we need to visit it
            self.visit_type_scheme(ty_scheme, tree)?;
        }

        if let Some(value) = value {
            self.visit_expr(value, tree)?;
        }

        self.scopes.type_.restore_depth(depth);
        self.bindings.value = None;

        ControlFlow::Continue(())
    }

    fn visit_let_expr(&mut self, id: Id<node::LetExpr>, tree: &Tree) -> ControlFlow<Self::BreakValue> {
        let node::LetExpr {
            name,
            value_type,
            value,
            body,
        } = *tree.get(id);

        let name = *tree.get(name);

        if let Some(type_) = value_type {
            self.visit_type_expr(type_, tree)?;
        }

        self.visit_expr(value, tree)?;

        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        self.scopes.value.enter(name, sym);
        self.visit_expr(body, tree)?;
        self.scopes.value.exit(&name);

        ControlFlow::Continue(())
    }

    fn visit_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr {
            param,
            param_type,
            body,
        } = *tree.get(id);

        let name = *tree.get(param);

        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        if let Some(param_type) = param_type {
            self.visit_type_expr(param_type, tree)?;
        }

        self.scopes.value.enter(name, sym);
        self.visit_expr(body, tree)?;
        self.scopes.value.exit(&name);

        ControlFlow::Continue(())
    }

    fn visit_case_branch(
        &mut self,
        id: Id<node::CaseBranch>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let depth = self.scopes.value.depth();
        self.walk_case_branch(id, tree)?;
        self.scopes.value.restore_depth(depth); // Restore former scope if patterns created binds

        ControlFlow::Continue(())
    }

    fn visit_handler_clause(
        &mut self,
        id: Id<node::HandlerClause>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::HandlerClause { op: _, param, body } = *tree.get(id);

        let param_name = *param.get(tree);
        let param_sym = ValueSym::new();

        self.insert_symbol(id, param_sym);
        self.scopes.value.enter(param_name, param_sym);

        self.visit_expr(body, tree)?;

        self.scopes.value.exit(&param_name);

        ControlFlow::Continue(())
    }

    fn visit_bind_pat(&mut self, id: Id<node::BindPat>, tree: &Tree) -> ControlFlow<Self::BreakValue> {
        let name = *id.get(tree).0.get(tree);
        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        self.scopes.value.enter(name, sym);
        ControlFlow::Continue(())
    }

    fn visit_list_pat(&mut self, id: Id<node::ListPat>, tree: &Tree) -> ControlFlow<Self::BreakValue> {
        let elements = &id.get(tree).0;

        for el in elements.get(tree).iter().copied() {
            match *el.get(tree) {
                node::ListElPat::Pat(pat) => self.visit_pat(pat, tree)?,
                node::ListElPat::Spread(Some(name_id)) => {
                    let name = *name_id.get(tree);
                    let sym = ValueSym::new();
                    self.insert_symbol(el, sym);

                    self.scopes.value.enter(name, sym);
                }
                _ => {
                    // Spread without name or empty list element - no action needed
                }
            }
        }

        ControlFlow::Continue(())
    }

    fn visit_record_pat(
        &mut self,
        id: Id<node::RecordPat>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordPat { fields, .. } = tree.get(id);

        for field_id in fields.get(tree).iter().copied() {
            let node::RecordFieldPat { field, pat } = *field_id.get(tree);

            if let Some(pat) = pat {
                self.visit_pat(pat, tree)?;
            } else {
                // No pattern means a binding: { a } => ...

                let name = *field.get(tree);
                let sym = ValueSym::new();
                self.insert_symbol(field_id, sym);

                self.scopes.value.enter(name, sym);
            }
        }

        ControlFlow::Continue(())
    }

    fn visit_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        // Actually I think nothing needs to be done here, for example:
        // < Some : { a } > => ...
        // This will create a binding but the binding part is handled by the Pat visitor,
        // nothing inside the VariantPat does any kind of binding.
        // However I suspect that I will need to revisit this for some kind of variant name analysis,
        // so I am keeping this here for now.

        self.walk_variant_pat(id, tree)
    }

    fn visit_qualified_expr(
        &mut self,
        id: Id<node::QualifiedExpr>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedExpr {
            module_path,
            source,
            ..
        } = *tree.get(id);

        let name = *source.get(tree);

        if let Some(path) = module_path {
            // Just visit the module path if it exists
            self.visit_module_path(path, tree)
        } else if let Some(value_sym) = self.scopes.value.get(&name) {
            // Local binding will not create value bind cycle either
            self.insert_symbol(id, ResolvedValue::Reference(*value_sym));
            ControlFlow::Continue(())
        } else if let Some(binding) = self.scope.names.get_value(name) {
            // Found a value binding in the current module scope
            // which was defined before this path expression (no forward reference)

            self.insert_symbol(id, ResolvedValue::Reference(binding.sym));

            let current_sym = self.bindings.value.unwrap();
            self.value_graph_map[&self.root].add_dependency(current_sym, binding.sym);

            ControlFlow::Continue(())
        } else if let Some(builtin) = self.interner.get(name.0).and_then(find_builtin_id) {
            // This is a builtin value - resolve immediately, no dependencies needed
            self.insert_symbol(id, ResolvedValue::Builtin(builtin));
            ControlFlow::Continue(())
        } else {
            // Either a forward reference or not found in the current module scope

            let current_sym = self.bindings.value.unwrap();
            let lookup = ValueLookup::new(name, id, current_sym, self.span(id), self.root);

            self.lookups.insert_value(lookup);

            ControlFlow::Continue(())
        }
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind {
            vis,
            name,
            ty_scheme,
        } = *tree.get(id);

        let vis = *vis.get(tree);
        let name = *tree.get(name);
        let loc = self.span(id);

        let type_sym = TypeSym::new();
        self.insert_symbol(id, type_sym);
        self.bindings.type_ = Some(type_sym);
        self.type_graph_map[&self.root].add_node(type_sym);

        // Register the type binding in the current scope
        self.insert_type(id, loc, vis, name, type_sym);

        // Intuitively I would guess that this is always 0
        // and I could just create a fresh type scope for every type and value bind.
        let depth = self.scopes.type_.depth();

        self.visit_type_scheme(ty_scheme, tree)?; // walk to discover module paths in type expressions

        self.scopes.type_.restore_depth(depth);
        self.bindings.type_ = None;

        ControlFlow::Continue(())
    }

    // gets called by with and forall binders
    fn visit_type_var_bind(
        &mut self,
        id: Id<node::TypeVarBind>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeVarBind { var, .. } = *id.get(tree);

        let name = var.get(tree).0.into();
        let sym = TypeSym::new();

        self.insert_symbol(id, sym);
        self.scopes.type_.enter(name, sym);

        ControlFlow::Continue(())
    }

    fn visit_qualified_type(
        &mut self,
        id: Id<node::QualifiedType>,
        tree: &Tree,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedType { path, ty } = *tree.get(id);

        let name = *ty.get(tree);

        let loc = self.span(id);

        if let Some(path) = path {
            // Just visit the module path if it exists
            self.visit_module_path(path, tree)
        } else if let Some(binding) = self.scope.names.get_type(name) {
            // Found a type binding in the current module scope
            // which was defined before this type path (no forward reference)

            self.insert_symbol(id, ResolvedType::Reference(binding.sym));

            // Type Path's can occur in both type binds and type annotations.
            // Only in the former case we need to add a dependency.
            if let Some(current_sym) = self.bindings.type_ {
                // Add dependency from the current type bind to this type
                self.type_graph_map[&self.root].add_dependency(current_sym, binding.sym);
            }

            ControlFlow::Continue(())
        } else if let Some(type_sym) = self.scopes.type_.get(&name) {
            // Local quantifier will not create type bind cycle either
            self.insert_symbol(id, ResolvedType::Reference(*type_sym));
            ControlFlow::Continue(())
        } else if let Some(builtin) = self.interner.get(name.0).and_then(BuiltinType::from_name) {
            // This is a builtin type - resolve immediately, no dependencies needed
            self.insert_symbol(id, ResolvedType::Builtin(builtin));
            ControlFlow::Continue(())
        }
        // Either a forward reference or not found in the current module scope
        else if let Some(current_sym) = self.bindings.type_ {
            let lookup = TypeLookup::new(name, id, current_sym, loc, self.root);
            self.lookups.insert_type(lookup);

            ControlFlow::Continue(())
        } else {
            let lookup = TypeAnnotLookup::new(name, id, loc, self.root);
            self.lookups.insert_type_annot(lookup);

            ControlFlow::Continue(())
        }
    }
}
