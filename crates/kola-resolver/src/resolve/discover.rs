/**
Let’s define:

- **Modules**: Each module is a node in a dependency graph.
- **Visiting State**: Each module can be in one of three states:
  - **Unvisited**: Not yet started.
  - **Visiting**: Currently being resolved (on the call stack).
  - **Visited**: Fully resolved.

### Algorithm Steps

1. **Start** at the root module.
2. For each submodule or inline module definition:
   - If the module is **Unvisited**, begin resolving it (mark as Visiting).
   - If the module is **Visiting**, report a cycle error and abort.
   - If the module is **Visited**, do nothing (already resolved).
3. **When resolving a module**:
   - For each reference to another module (e.g., `super::a`):
     - If the referenced module is **Unvisited**, recursively resolve it first.
     - If the referenced module is **Visiting**, report a cycle error.
     - If the referenced module is **Visited**, continue.
   - Once all dependencies are resolved, mark the current module as **Visited**.

---

## When Does This Algorithm Reject Sibling/Cousin References?

- **It only rejects a reference if it would create a cycle in the current call stack.**
- **Sibling/cousin references are accepted as long as they do not create a cycle.**
- **Mutual recursion is not allowed**: If two modules reference each other (directly or indirectly), a cycle is detected.
*/
use std::ops::ControlFlow;

use indexmap::IndexMap;
use kola_builtins::{BuiltinType, find_builtin_id};
use kola_span::{Loc, Report};
use kola_syntax::prelude::*;
use kola_tree::{
    node::{FunctorName, ModuleName, ModuleTypeName, TypeName, ValueName},
    prelude::*,
};
use kola_utils::{interner::StrInterner, scope::LinearScope};

use crate::{
    constraints::{
        GlobalConstraints, LocalConstraints, ModuleBindConst, ModuleConst, ModuleTypeBindConst,
        ModuleTypeConst, TypeBindConst, TypeConst, ValueConst,
    },
    def::{AnyDef, Def, DefMap, FunctorDef, ModuleDef, ModuleTypeDef, TypeDef, ValueDef},
    env::{Functor, FunctorMap, Module, ModuleMap},
    error::name_collision,
    phase::{ResolvePhase, ResolvedModule, ResolvedModuleType, ResolvedType, ResolvedValue},
    resolve::{FileMap, ModuleGraph, ModuleTypeGraph, TypeGraph, ValueGraph},
    symbol::{FunctorSym, ModuleSym, ModuleTypeSym, TypeSym, ValueSym},
};

#[derive(Debug, Clone)]
pub struct DiscoverOutput {
    pub cons: LocalConstraints,
    pub report: Report,
}

pub fn discover(
    sym: ModuleSym,
    tree: &Tree,
    locs: &LocVec,
    files: &FileMap,
    interner: &StrInterner,
    modules: &mut ModuleMap,
    functors: &mut FunctorMap,
    entry_points: &mut Vec<ValueSym>,
    defs: &mut DefMap,
    value_graph_map: &mut IndexMap<ModuleSym, ValueGraph>,
    type_graph_map: &mut IndexMap<ModuleSym, TypeGraph>,
    module_type_graph_map: &mut IndexMap<ModuleSym, ModuleTypeGraph>,
    module_graph: &mut ModuleGraph,
    global_cons: &mut GlobalConstraints,
) -> DiscoverOutput {
    let loc = *locs.meta(tree.root_id());

    value_graph_map.insert(sym, ValueGraph::new());
    type_graph_map.insert(sym, TypeGraph::new());
    module_type_graph_map.insert(sym, ModuleTypeGraph::new());

    // Create a visitor to walk the tree and collect declarations
    let mut discoverer = Discoverer::new(
        sym,
        loc,
        locs,
        files,
        interner,
        modules,
        functors,
        entry_points,
        defs,
        value_graph_map,
        type_graph_map,
        module_type_graph_map,
        module_graph,
        global_cons,
    );

    ControlFlow::Continue(()) = tree.root_id().visit_by(&mut discoverer, tree);

    DiscoverOutput {
        cons: discoverer.cons,
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
    scope: Module,
    cons: LocalConstraints,
    global_cons: &'a mut GlobalConstraints,
    bindings: Bindings,
    scopes: Scopes,
    report: Report,
    locs: &'a LocVec,
    files: &'a FileMap,
    interner: &'a StrInterner,
    modules: &'a mut ModuleMap,
    functors: &'a mut FunctorMap,
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
        loc: Loc,
        locs: &'a LocVec,
        files: &'a FileMap,
        interner: &'a StrInterner,
        modules: &'a mut ModuleMap,
        functors: &'a mut FunctorMap,
        entry_points: &'a mut Vec<ValueSym>,
        defs: &'a mut DefMap,
        value_graph_map: &'a mut IndexMap<ModuleSym, ValueGraph>,
        type_graph_map: &'a mut IndexMap<ModuleSym, TypeGraph>,
        module_type_graph_map: &'a mut IndexMap<ModuleSym, ModuleTypeGraph>,
        module_graph: &'a mut ModuleGraph,
        global_cons: &'a mut GlobalConstraints,
    ) -> Self {
        Self {
            root,
            scope: Module::new(loc),
            cons: LocalConstraints::new(),
            global_cons,
            bindings: Bindings::new(),
            scopes: Scopes::default(),
            report: Report::new(),
            locs,
            files,
            interner,
            modules,
            functors,
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
    fn span<T>(&self, id: Id<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        *self.locs.meta(id)
    }

    fn insert_symbol<T>(&mut self, id: Id<T>, sym: T::Meta)
    where
        T: MetaCast<ResolvePhase>,
    {
        self.scope.nodes.insert(id.as_usize(), T::upcast(sym));
    }

    pub fn insert_functor(&mut self, name: FunctorName, sym: FunctorSym, def: FunctorDef) {
        if let Some(bind) = self
            .scope
            .names
            .get(name)
            .and_then(|sym| self.defs.get(sym))
        {
            self.name_collision(def, bind);
            return;
        }

        self.scope.names.insert_functor(name, sym);
        self.defs.insert_functor(sym, def);
    }

    pub fn insert_module_type(
        &mut self,
        name: ModuleTypeName,
        sym: ModuleTypeSym,
        def: ModuleTypeDef,
    ) {
        if let Some(bind) = self
            .scope
            .names
            .get(name)
            .and_then(|sym| self.defs.get(sym))
        {
            self.name_collision(def, bind);
            return;
        }

        self.scope.names.insert_module_type(name, sym);
        self.defs.insert_module_type(sym, def);
    }

    pub fn insert_module(&mut self, name: ModuleName, sym: ModuleSym, def: ModuleDef) {
        if let Some(bind) = self
            .scope
            .names
            .get(name)
            .and_then(|sym| self.defs.get(sym))
        {
            self.name_collision(def, bind);
            return;
        }

        self.scope.names.insert_module(name, sym);
        self.defs.insert_module(sym, def);
    }

    pub fn insert_type(&mut self, name: TypeName, sym: TypeSym, def: TypeDef) {
        if let Some(bind) = self
            .scope
            .names
            .get(name)
            .and_then(|sym| self.defs.get(sym))
        {
            self.name_collision(def, bind);
            return;
        }

        self.scope.names.insert_type(name, sym);
        self.defs.insert_type(sym, def);
    }

    pub fn insert_value(&mut self, name: ValueName, sym: ValueSym, def: ValueDef) {
        if let Some(bind) = self
            .scope
            .names
            .get(name)
            .and_then(|sym| self.defs.get(sym))
        {
            self.name_collision(def, bind);
            return;
        }

        self.scope.names.insert_value(name, sym);
        self.defs.insert_value(sym, def);
    }

    fn with_fresh_scope<T, F>(&mut self, child: ModuleSym, loc: Loc, f: F) -> (T, Module)
    where
        F: FnOnce(&mut Self) -> T,
    {
        let parent = self.root;

        // Skip adding dependencies if child is the same as parent,
        // which should only happen in the root module
        if child != parent {
            self.module_graph.add_dependency(parent, child);
            self.value_graph_map.insert(child, ValueGraph::new());
            self.type_graph_map.insert(child, TypeGraph::new());
            self.module_type_graph_map
                .insert(child, ModuleTypeGraph::new());
        }

        let saved = self.bindings.replace();

        let mut child_scope = Module::new(loc);
        std::mem::swap(&mut child_scope, &mut self.scope);
        self.root = child;

        let result = f(self);

        std::mem::swap(&mut child_scope, &mut self.scope);
        self.root = parent;
        self.bindings = saved;

        (result, child_scope)
    }
}

impl<'a, T> Visitor<T> for Discoverer<'a>
where
    T: TreeView,
{
    type BreakValue = !;

    fn visit_functor_bind(&mut self, id: Id<node::FunctorBind>, tree: &T) -> ControlFlow<!> {
        let node::FunctorBind {
            vis,
            name,
            params,
            body,
        } = tree.node(id);

        let loc = self.span(id);
        let name = *name.get(tree);

        // Visit parameter types in parent scope
        for param in params {
            self.visit_module_type(tree.node(*param).ty, tree)?;
        }

        // Functor body gets its own fresh scope
        let local_cons = std::mem::take(&mut self.cons);
        let global_cons = std::mem::take(self.global_cons);
        let prototype = ModuleSym::new();
        let (param_syms, body_scope) = self.with_fresh_scope(prototype, loc, |this| {
            // Insert parameter modules into the fresh scope
            let mut param_syms = Vec::with_capacity(params.len());

            for param in params {
                let node::FunctorParam {
                    name: param_name, ..
                } = *tree.node(*param);
                let param_name = *param_name.get(tree);
                let param_sym = ModuleSym::new();

                this.scope.names.insert_module(param_name, param_sym);
                param_syms.push(param_sym);
            }

            // Walk the body in the fresh scope
            let _ = this.walk_module(*body, tree);

            param_syms
        });

        let sym = FunctorSym::new();
        self.insert_symbol(id, sym);
        self.insert_functor(name, sym, Def::new(id, *vis.get(tree), loc));

        let local_cons = std::mem::replace(&mut self.cons, local_cons);
        let global_cons = std::mem::replace(self.global_cons, global_cons);

        let functor = Functor::new(prototype, param_syms, body_scope, local_cons, global_cons);
        self.functors.insert(sym, functor);
        ControlFlow::Continue(())
    }

    fn visit_functor_args(
        &mut self,
        id: Id<node::FunctorArgs>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let args = id.get(tree);

        let mut arg_syms = Vec::with_capacity(args.0.len());

        for arg_id in args {
            let arg_sym = ModuleSym::new();
            self.bindings.module = Some(arg_sym);

            self.visit_module_path(*arg_id, tree)?;
            arg_syms.push(arg_sym);
        }
        self.insert_symbol(id, arg_syms);

        ControlFlow::Continue(())
    }

    fn visit_functor_app(
        &mut self,
        id: Id<node::FunctorApp>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorApp { path, func, args } = *tree.node(id);

        let name = *func.get(tree);
        let loc = self.span(id);

        let bind = self.bindings.module.take().unwrap();
        self.scope.nodes.insert_meta(id, bind);

        let path = if let Some(path) = path {
            let bind = ModuleSym::new();
            self.bindings.module = Some(bind);

            self.visit_module_path(path, tree)?;

            Some(bind)
        } else {
            None
        };

        self.visit_functor_args(args, tree)?;
        let arg_syms = self.scope.nodes.meta(args).clone();

        let constraint = ModuleBindConst::functor(id, self.root, bind, path, loc, name, arg_syms);
        self.global_cons.push_back(constraint);

        ControlFlow::Continue(())
    }

    fn visit_module_type_bind(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleTypeBind { vis, name, .. } = *tree.node(id);

        let vis = *tree.node(vis);
        let name = *tree.node(name);

        let span = self.span(id);

        let module_type_sym = ModuleTypeSym::new();
        self.insert_symbol(id, module_type_sym);
        self.bindings.module_type = Some(module_type_sym);

        // Register the module type binding in the current scope
        self.insert_module_type(name, module_type_sym, Def::new(id, vis, span));
        self.walk_module_type_bind(id, tree)?;

        self.bindings.module_type = None;

        ControlFlow::Continue(())
    }

    fn visit_qualified_module_type(
        &mut self,
        id: Id<node::QualifiedModuleType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedModuleType { path, ty } = *tree.node(id);

        let name = *ty.get(tree);
        let loc = self.span(id);

        if let Some(path) = path {
            // Module Type defined in another module - Just visit the module path if it exists
            self.visit_module_path(path, tree)?;
        } else if let Some(sym) = self.scope.names.get_module_type(name) {
            // Found a module type binding in the current module scope
            // which was defined before this reference to it (no forward reference)

            self.insert_symbol(id, ResolvedModuleType(sym));

            // Qualified Module Types can occur in both module type binds and module type annotations.
            // Only in the former case we need to add a dependency.
            if let Some(current_sym) = self.bindings.module_type {
                // Add dependency from the current type bind to this type
                self.module_type_graph_map[&self.root].add_dependency(current_sym, sym);
            }
        }
        // Either a forward reference or not found in the current module scope
        else if let Some(current_sym) = self.bindings.module_type {
            let ref_ = ModuleTypeBindConst::new(name, id, current_sym, loc);
            self.cons.insert_module_type_bind(ref_);
        } else {
            let ref_ = ModuleTypeConst::new(name, id, loc);
            self.cons.insert_module_type(ref_);
        }
        ControlFlow::Continue(())
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleBind { vis, name, .. } = *tree.node(id);

        let name = *tree.node(name);
        let vis = tree.node(vis);

        let span = self.span(id);

        let module_sym = ModuleSym::new();
        self.insert_symbol(id, module_sym);
        self.bindings.module = Some(module_sym);

        // Register the module binding in the current scope
        self.insert_module(name, module_sym, Def::new(id, *vis, span));
        self.walk_module_bind(id, tree)
    }

    fn visit_module(&mut self, id: Id<node::Module>, tree: &T) -> ControlFlow<Self::BreakValue> {
        // Either inline module or root module
        let sym = self.bindings.module.take().unwrap_or(self.root);
        let loc = self.span(id);

        let (_, module) = self.with_fresh_scope(sym, loc, |this| {
            let _ = this.walk_module(id, tree);
        });

        self.modules.insert(sym, module);

        ControlFlow::Continue(())
    }

    fn visit_module_import(
        &mut self,
        id: Id<node::ModuleImport>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // Just remove the module bindings symbol,
        // we use the file map to retrieve the actual symbol
        self.bindings.module.take();

        let target = id.get(tree).0;
        let sym = self.files[&target];

        self.insert_symbol(id, sym);
        self.module_graph.add_dependency(self.root, sym);

        ControlFlow::Continue(())
    }

    fn visit_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let path = tree
            .node(id)
            .0
            .iter()
            .copied()
            .map(|id| *tree.node(id))
            .collect::<Vec<_>>();

        let loc = self.span(id);

        if let Some(bind) = self.bindings.module.take() {
            // If this is called from a module bind, we can insert the module path
            // module a = b::c

            self.insert_symbol(id, ResolvedModule(bind));

            let constraint = ModuleBindConst::path(id, self.root, bind, loc, path);
            self.global_cons.push_back(constraint);
        } else {
            // If we don't have a current module bind, we need to create a reference
            // module::record.field

            self.cons
                .insert_module(ModuleConst::new(path, id, self.root, self.span(id)));
        }

        ControlFlow::Continue(())
    }

    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueBind {
            vis,
            name,
            ty_scheme,
            value,
        } = *tree.node(id);

        let name = *tree.node(name);
        let vis = tree.node(vis);

        let span = self.span(id);

        let value_sym = ValueSym::new();
        self.insert_symbol(id, value_sym);
        self.bindings.value = Some(value_sym);
        self.value_graph_map[&self.root].add_node(value_sym);

        if self.interner.get(name.0) == Some("main") {
            // If this is the main entry point, we will collect it later
            self.entry_points.push(value_sym);
        }

        // Register the value binding in the current scope
        self.insert_value(name, value_sym, Def::new(id, *vis, span));

        // Intuitively I would guess that this is always 0
        // and I could just create a fresh type scope for every type and value bind.
        let depth = self.scopes.type_.depth();

        if let Some(ty_scheme) = ty_scheme {
            // If there is a type annotation, we need to visit it
            self.visit_type_scheme(ty_scheme, tree)?;
        }

        self.visit_expr(value, tree)?;

        self.scopes.type_.restore_depth(depth);
        self.bindings.value = None;

        ControlFlow::Continue(())
    }

    fn visit_let_expr(&mut self, id: Id<node::LetExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let node::LetExpr {
            name,
            value_type,
            value,
            body,
        } = *tree.node(id);

        let name = *tree.node(name);

        if let Some(type_) = value_type {
            self.visit_type(type_, tree)?;
        }

        ValueSym::enter();
        self.visit_expr(value, tree)?;
        ValueSym::exit();

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
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr {
            param,
            param_type,
            body,
        } = *tree.node(id);

        let name = *tree.node(param);

        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        if let Some(param_type) = param_type {
            self.visit_type(param_type, tree)?;
        }

        self.scopes.value.enter(name, sym);
        self.visit_expr(body, tree)?;
        self.scopes.value.exit(&name);

        ControlFlow::Continue(())
    }

    fn visit_case_branch(
        &mut self,
        id: Id<node::CaseBranch>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let depth = self.scopes.value.depth();
        self.walk_case_branch(id, tree)?;
        self.scopes.value.restore_depth(depth); // Restore former scope if patterns created binds

        ControlFlow::Continue(())
    }

    fn visit_handler_clause(
        &mut self,
        id: Id<node::HandlerClause>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::HandlerClause { op: _, param, body } = *tree.node(id);

        let param_name = *param.get(tree);
        let param_sym = ValueSym::new();

        self.insert_symbol(id, param_sym);
        self.scopes.value.enter(param_name, param_sym);

        self.visit_expr(body, tree)?;

        self.scopes.value.exit(&param_name);

        ControlFlow::Continue(())
    }

    fn visit_bind_pat(&mut self, id: Id<node::BindPat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let name = *id.get(tree).0.get(tree);
        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        self.scopes.value.enter(name, sym);
        ControlFlow::Continue(())
    }

    fn visit_list_pat(&mut self, id: Id<node::ListPat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let elements = &id.get(tree).0;

        for &el in elements {
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
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::RecordPat { fields, .. } = tree.node(id);

        for &field_id in fields {
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
        tree: &T,
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
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedExpr {
            module_path,
            source,
            ..
        } = *tree.node(id);

        let name = *source.get(tree);

        if let Some(path) = module_path {
            // Just visit the module path if it exists
            self.visit_module_path(path, tree)
        } else if let Some(value_sym) = self.scopes.value.get(&name) {
            // Local binding will not create value bind cycle either
            self.insert_symbol(id, ResolvedValue::Reference(*value_sym));
            ControlFlow::Continue(())
        } else if let Some(value_sym) = self.scope.names.get_value(name) {
            // Found a value binding in the current module scope
            // which was defined before this path expression (no forward reference)

            self.insert_symbol(id, ResolvedValue::Reference(value_sym));

            let current_sym = self.bindings.value.unwrap();
            self.value_graph_map[&self.root].add_dependency(current_sym, value_sym);

            ControlFlow::Continue(())
        } else if let Some(builtin) = self.interner.get(name.0).and_then(find_builtin_id) {
            // This is a builtin value - resolve immediately, no dependencies needed
            self.insert_symbol(id, ResolvedValue::Builtin(builtin));
            ControlFlow::Continue(())
        } else {
            // Either a forward reference or not found in the current module scope

            let current_sym = self.bindings.value.unwrap();
            let ref_ = ValueConst::new(name, id, current_sym, self.span(id));

            self.cons.insert_value(ref_);

            ControlFlow::Continue(())
        }
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind {
            vis,
            name,
            ty_scheme,
        } = *tree.node(id);

        let vis = *vis.get(tree);
        let name = *tree.node(name);
        let span = self.span(id);

        let type_sym = TypeSym::new();
        self.insert_symbol(id, type_sym);
        self.bindings.type_ = Some(type_sym);
        self.type_graph_map[&self.root].add_node(type_sym);

        // Register the type binding in the current scope
        self.insert_type(name, type_sym, Def::new(id, vis, span));

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
        tree: &T,
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
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedType { path, ty } = *tree.node(id);

        let name = *ty.get(tree);

        let loc = self.span(id);

        if let Some(path) = path {
            // Just visit the module path if it exists
            self.visit_module_path(path, tree)
        } else if let Some(type_sym) = self.scope.names.get_type(name) {
            // Found a type binding in the current module scope
            // which was defined before this type path (no forward reference)

            self.insert_symbol(id, ResolvedType::Reference(type_sym));

            // Type Path's can occur in both type binds and type annotations.
            // Only in the former case we need to add a dependency.
            if let Some(current_sym) = self.bindings.type_ {
                // Add dependency from the current type bind to this type
                self.type_graph_map[&self.root].add_dependency(current_sym, type_sym);
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
            let ref_ = TypeBindConst::new(name, id, current_sym, loc);
            self.cons.insert_type_bind(ref_);

            ControlFlow::Continue(())
        } else {
            let ref_ = TypeConst::qualified(name, id, loc);
            self.cons.insert_type(ref_);

            ControlFlow::Continue(())
        }
    }
}
