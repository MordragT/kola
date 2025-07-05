use camino::Utf8Path;
use log::debug;
use owo_colors::OwoColorize;
use std::{collections::HashMap, io, ops::ControlFlow};

use kola_builtins::{BuiltinEffect, BuiltinType, find_builtin_id};
use kola_print::prelude::*;
use kola_span::{Diagnostic, IntoDiagnostic, Loc, Report, SourceId, SourceManager};
use kola_syntax::prelude::*;
use kola_tree::{node::Vis, prelude::*};
use kola_utils::{interner::StrInterner, io::FileSystem};

use crate::{
    GlobalId,
    constraints::{
        EffectBindConst, EffectConst, ModuleBindConst, ModuleConst, ModuleTypeBindConst,
        ModuleTypeConst, TypeBindConst, TypeConst, ValueConst,
    },
    defs::Def,
    forest::Forest,
    functor::Functor,
    info::ModuleInfo,
    phase::{
        ResolvePhase, ResolvedEffect, ResolvedModule, ResolvedModuleType, ResolvedType,
        ResolvedValue,
    },
    prelude::Topography,
    scope::{ModuleScope, ModuleScopeStack},
    symbol::{EffectSym, FunctorSym, ModuleSym, ModuleTypeSym, TypeSym, ValueSym},
};

use super::ModuleGraph;

#[derive(Debug, Clone, Default)]
pub struct DiscoverOutput {
    pub source_manager: SourceManager,
    pub forest: Forest,
    pub topography: Topography,
    pub module_graph: ModuleGraph,
    pub module_scopes: Vec<ModuleScope>,
    pub functors: HashMap<FunctorSym, Functor>,
    pub entry_points: Vec<ValueSym>,
}

pub fn discover(
    path: impl AsRef<Utf8Path>,
    io: &dyn FileSystem,
    arena: &Bump,
    interner: &mut StrInterner,
    report: &mut Report,
    print_options: PrintOptions,
) -> io::Result<DiscoverOutput> {
    let mut source_manager = SourceManager::new();
    let path = io.canonicalize(path.as_ref())?;
    let (source_id, source) = source_manager.fetch(path.as_path(), &io)?;

    debug!(
        "{} {}\n{}",
        "Source".bold().bright_white(),
        &path,
        source.text()
    );

    let input = LexInput::new(source_id, source.text());
    let Some(tokens) = tokenize(input, report) else {
        return Ok(DiscoverOutput {
            source_manager,
            ..Default::default()
        });
    };

    debug!(
        "{} {:?}\n{}",
        "Tokens".bold().bright_white(),
        &path,
        TokenPrinter(&tokens, print_options).render(print_options, arena)
    );

    let input = ParseInput::new(source_id, tokens, source.len());
    let ParseOutput { tree, spans, .. } = parse(input, interner, report);

    let Some(tree) = tree else {
        return Ok(DiscoverOutput {
            source_manager,
            ..Default::default()
        });
    };

    {
        // let loc_decorator = LocDecorator(&spans);
        // let decorators = Decorators::new().with(&loc_decorator);
        let decorators = Decorators::new();
        let tree_printer = TreePrinter::root(&tree, interner, decorators);

        debug!(
            "{} {:?}\n{}",
            "Untyped Abstract Syntax Tree".bold().bright_white(),
            &path,
            tree_printer.render(print_options, arena)
        );
    }

    let mut forest = Forest::new();
    let mut topography = Topography::new();
    forest.insert(source_id, tree);
    topography.insert(source_id, spans);

    if !report.is_empty() {
        // If there are errors in parsing the imported module, we skip discovery
        return Ok(DiscoverOutput {
            source_manager,
            forest,
            topography,
            ..Default::default()
        });
    }

    let mut module_graph = ModuleGraph::new();
    let mut module_scopes = Vec::new();
    let mut functors = HashMap::new();
    let mut entry_points = Vec::new();

    _discover(
        source_id,
        ModuleSym::new(),
        io,
        arena,
        interner,
        report,
        &mut source_manager,
        &mut forest,
        &mut topography,
        &mut module_graph,
        &mut module_scopes,
        &mut functors,
        &mut entry_points,
        print_options,
    );

    Ok(DiscoverOutput {
        source_manager,
        forest,
        topography,
        module_graph,
        module_scopes,
        functors,
        entry_points,
    })
}

fn _discover(
    source_id: SourceId,
    module_sym: ModuleSym,
    io: &dyn FileSystem,
    arena: &Bump,
    interner: &mut StrInterner,
    report: &mut Report,
    source_manager: &mut SourceManager,
    forest: &mut Forest,
    topography: &mut Topography,
    module_graph: &mut ModuleGraph,
    module_scopes: &mut Vec<ModuleScope>,
    functors: &mut HashMap<FunctorSym, Functor>,
    entry_points: &mut Vec<ValueSym>,
    print_options: PrintOptions,
) {
    let tree = forest.tree(source_id);

    // Create a visitor to walk the tree and collect declarations
    let mut discoverer = Discoverer::new(
        source_id,
        module_sym,
        io,
        arena,
        interner,
        report,
        source_manager,
        forest,
        topography,
        module_graph,
        module_scopes,
        functors,
        entry_points,
        print_options,
    );

    ControlFlow::Continue(()) = tree.root_id().visit_by(&mut discoverer, &*tree);
}

struct Discoverer<'a> {
    source_id: SourceId,
    current_module_type_bind_sym: Option<ModuleTypeSym>,
    current_module_bind_sym: Option<ModuleSym>,
    current_effect_type_bind_sym: Option<EffectSym>,
    current_type_bind_sym: Option<TypeSym>,
    current_value_bind_sym: Option<ValueSym>,
    stack: ModuleScopeStack,
    io: &'a dyn FileSystem,
    arena: &'a Bump,
    interner: &'a mut StrInterner,
    report: &'a mut Report,
    source_manager: &'a mut SourceManager,
    forest: &'a mut Forest,
    topography: &'a mut Topography,
    module_graph: &'a mut ModuleGraph,
    module_scopes: &'a mut Vec<ModuleScope>,
    functors: &'a mut HashMap<FunctorSym, Functor>,
    entry_points: &'a mut Vec<ValueSym>,
    print_options: PrintOptions,
}

impl<'a> Discoverer<'a> {
    fn new(
        source_id: SourceId,
        module_sym: ModuleSym,
        io: &'a dyn FileSystem,
        arena: &'a Bump,
        interner: &'a mut StrInterner,
        report: &'a mut Report,
        source_manager: &'a mut SourceManager,
        forest: &'a mut Forest,
        topography: &'a mut Topography,
        module_graph: &'a mut ModuleGraph,
        module_scopes: &'a mut Vec<ModuleScope>,
        functors: &'a mut HashMap<FunctorSym, Functor>,
        entry_points: &'a mut Vec<ValueSym>,
        print_options: PrintOptions,
    ) -> Self {
        Self {
            source_id,
            current_module_type_bind_sym: None,
            current_module_bind_sym: Some(module_sym),
            current_effect_type_bind_sym: None,
            current_type_bind_sym: None,
            current_value_bind_sym: None,
            stack: ModuleScopeStack::new(),
            io,
            arena,
            interner,
            report,
            source_manager,
            forest,
            topography,
            module_graph,
            module_scopes,
            functors,
            entry_points,
            print_options,
        }
    }

    #[inline]
    fn span<T>(&self, id: Id<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        self.topography.span(GlobalId::new(self.source_id, id))
    }

    fn insert_symbol<T>(&mut self, id: Id<T>, sym: T::Meta)
    where
        T: MetaCast<ResolvePhase>,
    {
        self.stack
            .resolved_mut()
            .insert(id.as_usize(), T::upcast(sym));
    }
}

impl<'a, T> Visitor<T> for Discoverer<'a>
where
    T: TreeView,
{
    type BreakValue = !;

    fn visit_functor_bind(
        &mut self,
        id: Id<node::FunctorBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorBind {
            vis,
            name,
            param,
            param_ty,
            body,
        } = *tree.node(id);

        let name = *tree.node(name);
        let vis = *tree.node(vis);

        let loc = self.span(id);

        let sym = FunctorSym::new();
        self.insert_symbol(id, sym);

        if let Err(e) = self
            .stack
            .insert_functor(name, sym, Def::bound(id, vis, loc))
        {
            self.report.add_diagnostic(e.into());
        }

        // First, visit the parameter type annotation. This will add a ModuleTypeRef to refs (if not present yet),
        // which will be resolved in the module_ty resolution phase.
        // It has to be visited before the body, because types are defined in the parent scope not in the body scope.
        self.visit_module_type(param_ty, tree)?;

        // Create a new scope for the functor body with a new ModuleSym.
        let body_sym = ModuleSym::new();
        self.stack
            .start(ModuleInfo::new(body, body_sym, self.source_id, loc));

        dbg!(body_sym);

        // Create a new unique ModuleSym for the functor's parameter (e.g., 'x' in 'functor f x : S => ...').
        // This is the 'virtual' module that exists inside the functor's scope.
        let param_sym = ModuleSym::new();
        let param_loc = self.span(param);
        let param_name = *param.get(tree);

        dbg!(param_sym);

        // Insert the parameter module into the scope of the functor
        if let Err(e) =
            self.stack
                .insert_module(param_name, param_sym, Def::unbound(param_loc, Vis::Export))
        // TODO here we set Vis to export but ideally the functor should not "bleed" the parameter scope
        {
            self.report.add_diagnostic(e.into());
        }

        // No need to add dependency information here, because due to the walk_module call later,
        // nested bindings will automatically also insert into the dependency graphs.
        // The only problem is that the parameter creates a new ModuleSym,
        // which means that the dependency graphs do not contain the real parameter symbol but the mocked one.
        // So when applying the functor, there is a need for substitution of the parameter symbol
        // The functor symbol itsself does as far as I understand not need a edge from the parent module.

        // Now we can visit the functor body.
        // Note that this uses walk, because the stack is already set up for the functor body.
        self.walk_module(body, tree)?;

        // Pop the ModuleScope from the stack and create a new Definition for the functor bind
        let body = self.stack.finish();
        let functor = Functor::new(param_sym, body);

        self.functors.insert(sym, functor);

        ControlFlow::Continue(())
    }

    fn visit_functor_app(
        &mut self,
        id: Id<node::FunctorApp>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::FunctorApp { func, arg } = *tree.node(id);

        let name = *func.get(tree);
        let loc = self.span(id);

        let arg_sym = ModuleSym::new();
        let bind = self.current_module_bind_sym.replace(arg_sym).unwrap();
        self.stack.resolved_mut().insert_meta(id, bind);

        let node::ModuleExpr::Path(arg_id) = *tree.node(arg) else {
            // TODO for functor arguments other than module paths, the current module resolution implementation is insufficient
            // Most importantly scoping is not implemented correctly e.g.:
            //
            // module a = ...,
            // module b = ...,
            // module functor f x : ... => ...
            // module c = (f { module a = a, module b = b }) // This will not work, because the anonymous module has its own module scope
            //                                                  and doesn't inherit from its parent module
            //
            // Maybe for inline modules in general, scoping should be less strict and allow transparent use of parent bindings.
            self.report.add_diagnostic(
                Diagnostic::error(
                    loc,
                    "Functor application argument must currently be a module path",
                )
                .with_help("Use a module path to apply a functor."),
            );
            return ControlFlow::Continue(());
        };

        self.visit_module_path(arg_id, tree)?;

        let constraint = ModuleBindConst::functor(id, bind, loc, name, arg_sym);
        self.stack.cons_mut().insert_module_bind(bind, constraint);

        ControlFlow::Continue(())
    }

    fn visit_module_type_bind(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleTypeBind { name, .. } = *tree.node(id);

        let name = *tree.node(name);

        let span = self.span(id);

        let module_type_sym = ModuleTypeSym::new();
        self.insert_symbol(id, module_type_sym);
        self.current_module_type_bind_sym = Some(module_type_sym);

        // Register the module type binding in the current scope
        if let Err(e) =
            self.stack
                .insert_module_type(name, module_type_sym, Def::bound(id, Vis::Export, span))
        {
            self.report.add_diagnostic(e.into());
        }

        self.walk_module_type_bind(id, tree)?;

        self.current_module_type_bind_sym = None;

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
        } else if let Some(sym) = self.stack.shape().get_module_type(name) {
            // Found a module type binding in the current module scope
            // which was defined before this reference to it (no forward reference)

            self.insert_symbol(id, ResolvedModuleType(sym));

            // Qualified Module Types can occur in both module type binds and module type annotations.
            // Only in the former case we need to add a dependency.
            if let Some(current_sym) = self.current_module_type_bind_sym {
                // Add dependency from the current type bind to this type
                self.stack
                    .module_type_graph_mut()
                    .add_dependency(current_sym, sym);
            }
        }
        // Either a forward reference or not found in the current module scope
        else if let Some(current_sym) = self.current_module_type_bind_sym {
            let ref_ = ModuleTypeBindConst::new(name, id, current_sym, loc);
            self.stack.cons_mut().insert_module_type_bind(ref_);
        } else {
            let ref_ = ModuleTypeConst::new(name, id, loc);
            self.stack.cons_mut().insert_module_type(ref_);
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
        self.current_module_bind_sym = Some(module_sym);

        // Register the module binding in the current scope
        if let Err(e) = self
            .stack
            .insert_module(name, module_sym, Def::bound(id, *vis, span))
        {
            self.report.add_diagnostic(e.into());
        }

        self.walk_module_bind(id, tree)
    }

    fn visit_module(&mut self, id: Id<node::Module>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let sym = self.current_module_bind_sym.take().unwrap();

        // Add dependency from current module to this new module
        if let Some(info) = self.stack.try_info() {
            self.module_graph.add_dependency(info.sym, sym);
        } else {
            // If there is no current module, this is the root module
            // and we need to add it to the module graph
            self.module_graph.add_node(sym);
        }

        // Create a new scope for this module
        self.stack
            .start(ModuleInfo::new(id, sym, self.source_id, self.span(id)));

        self.insert_symbol(id, sym);

        // Walk through children nodes
        self.walk_module(id, tree)?;

        // Pop the scope now that we're done with this module
        self.module_scopes.push(self.stack.finish());

        ControlFlow::Continue(())
    }

    fn visit_module_import(
        &mut self,
        id: Id<node::ModuleImport>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let module_sym = self.current_module_bind_sym.take().unwrap();
        self.insert_symbol(id, module_sym);

        let current_module_sym = self.stack.info().sym;
        self.module_graph
            .add_dependency(current_module_sym, module_sym);

        let import = tree.node(id);
        let name = tree.node(import.0);

        let path = match self
            .source_manager
            .resolve_import(self.source_id, name.0, &self.interner)
        {
            Ok(path) => path,
            Err(e) => {
                self.report.add_diagnostic(e.into_diagnostic(self.span(id)));
                return ControlFlow::Continue(());
            }
        };

        let (source_id, source) = match self.source_manager.fetch(path.as_path(), &self.io) {
            Ok(tuple) => tuple,
            Err(e) => {
                self.report.add_diagnostic(e.into_diagnostic(self.span(id)));
                return ControlFlow::Continue(());
            }
        };

        debug!(
            "{} {}\n{}",
            "Source".bold().bright_white(),
            &path,
            source.text()
        );

        let input = LexInput::new(source_id, source.text());
        let Some(tokens) = tokenize(input, &mut self.report) else {
            return ControlFlow::Continue(());
        };

        debug!(
            "{} {:?}\n{}",
            "Tokens".bold().bright_white(),
            &path,
            TokenPrinter(&tokens, self.print_options).render(self.print_options, &self.arena)
        );

        let input = ParseInput::new(source_id, tokens, source.len());

        let ParseOutput { tree, spans, .. } = parse(input, &mut self.interner, &mut self.report);

        let Some(tree) = tree else {
            return ControlFlow::Continue(());
        };

        // let loc_decorator = LocDecorator(&spans);
        // let decorators = Decorators::new().with(&loc_decorator);
        let decorators = Decorators::new();
        let tree_printer = TreePrinter::root(&tree, &self.interner, decorators);

        debug!(
            "{} {:?}\n{}",
            "Untyped Abstract Syntax Tree".bold().bright_white(),
            &path,
            tree_printer.render(self.print_options, &self.arena)
        );

        self.forest.insert(source_id, tree);
        self.topography.insert(source_id, spans);

        if !self.report.is_empty() {
            // If there are errors in parsing the imported module, we skip discovery
            return ControlFlow::Continue(());
        }

        _discover(
            source_id,
            module_sym,
            self.io,
            self.arena,
            self.interner,
            self.report,
            self.source_manager,
            self.forest,
            self.topography,
            self.module_graph,
            self.module_scopes,
            self.functors,
            self.entry_points,
            self.print_options,
        );

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

        if let Some(bind) = self.current_module_bind_sym.take() {
            // If this is called from a module bind, we can insert the module path
            // module a = b::c

            self.insert_symbol(id, ResolvedModule(bind));

            let constraint = ModuleBindConst::path(id, bind, loc, path);
            self.stack.cons_mut().insert_module_bind(bind, constraint);
        } else {
            // If we don't have a current module bind, we need to create a reference
            // module::record.field

            let current_sym = self.stack.info().sym;
            let ref_ = ModuleConst::new(path, id, current_sym, self.span(id));

            self.stack.cons_mut().insert_module(ref_);
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
        self.current_value_bind_sym = Some(value_sym);
        self.stack.value_graph_mut().add_node(value_sym);

        if self.interner.get(name.0) == Some("main") {
            // If this is the main entry point, we will collect it later
            self.entry_points.push(value_sym);
        }

        // Register the value binding in the current scope
        if let Err(e) = self
            .stack
            .insert_value(name, value_sym, Def::bound(id, *vis, span))
        {
            self.report.add_diagnostic(e.into());
        }

        if let Some(ty_scheme) = ty_scheme {
            // If there is a type annotation, we need to visit it
            self.visit_type_scheme(ty_scheme, tree)?;
        }

        self.visit_expr(value, tree)?;

        self.current_value_bind_sym = None;

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

        self.stack.value_scope_mut().enter(name, sym);
        self.visit_expr(body, tree)?;
        self.stack.value_scope_mut().exit(&name);

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

        self.stack.value_scope_mut().enter(name, sym);
        self.visit_expr(body, tree)?;
        self.stack.value_scope_mut().exit(&name);

        ControlFlow::Continue(())
    }

    fn visit_case_branch(
        &mut self,
        id: Id<node::CaseBranch>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let depth = self.stack.value_scope_mut().depth();
        self.walk_case_branch(id, tree)?;
        self.stack.value_scope_mut().restore_depth(depth); // Restore former scope if patterns created binds

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
        self.stack.value_scope_mut().enter(param_name, param_sym);

        self.visit_expr(body, tree)?;

        self.stack.value_scope_mut().exit(&param_name);

        ControlFlow::Continue(())
    }

    // TODOs insert symbols

    fn visit_bind_pat(&mut self, id: Id<node::BindPat>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let name = *id.get(tree).0.get(tree);
        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        self.stack.value_scope_mut().enter(name, sym);
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

                    self.stack.value_scope_mut().enter(name, sym);
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

                self.stack.value_scope_mut().enter(name, sym);
            }
        }

        ControlFlow::Continue(())
    }

    fn visit_variant_pat(
        &mut self,
        id: Id<node::VariantPat>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // let tags = &id.get(tree).0;

        // for id in tags {
        //     let node::VariantTagPat { tag, pat } = *id.get(tree);

        //     if let Some(pat) = pat {
        //         self.visit_pat(pat, tree)?;
        //     } else {
        //         // No pattern means a binding: Variant { a } => ...

        //         let name = *name.get(tree);
        //         let sym = ValueSym::new();

        //         self.stack.value_scope_mut().enter(name, sym);
        //     }
        // }
        //

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
        } else if let Some(value_sym) = self.stack.value_scope().get(&name) {
            // Local binding will not create value bind cycle either
            self.insert_symbol(id, ResolvedValue::Reference(*value_sym));
            ControlFlow::Continue(())
        } else if let Some(value_sym) = self.stack.shape().get_value(name) {
            // Found a value binding in the current module scope
            // which was defined before this path expression (no forward reference)

            self.insert_symbol(id, ResolvedValue::Reference(value_sym));

            let current_sym = self.current_value_bind_sym.unwrap();
            self.stack
                .value_graph_mut()
                .add_dependency(current_sym, value_sym);

            ControlFlow::Continue(())
        } else if let Some(builtin) = self.interner.get(name.0).and_then(find_builtin_id) {
            // This is a builtin value - resolve immediately, no dependencies needed
            self.insert_symbol(id, ResolvedValue::Builtin(builtin));
            ControlFlow::Continue(())
        } else {
            // Either a forward reference or not found in the current module scope

            let current_sym = self.current_value_bind_sym.unwrap();
            let ref_ = ValueConst::new(name, id, current_sym, self.span(id));

            self.stack.cons_mut().insert_value(ref_);

            ControlFlow::Continue(())
        }
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind { name, ty_scheme } = *tree.node(id);

        let name = *tree.node(name);
        let span = self.span(id);

        let type_sym = TypeSym::new();
        self.insert_symbol(id, type_sym);
        self.current_type_bind_sym = Some(type_sym);
        self.stack.type_graph_mut().add_node(type_sym);

        // Register the type binding in the current scope
        if let Err(e) = self
            .stack
            .insert_type(name, type_sym, Def::bound(id, Vis::Export, span))
        {
            self.report.add_diagnostic(e.into());
        }

        self.visit_type_scheme(ty_scheme, tree)?; // walk to discover module paths in type expressions

        self.current_type_bind_sym = None;

        ControlFlow::Continue(())
    }

    fn visit_type_scheme(
        &mut self,
        id: Id<node::TypeScheme>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeScheme { vars, ty } = id.get(tree);

        // Enter type parameter scope
        for &var_id in vars {
            let name = var_id.get(tree).0.into();
            let sym = TypeSym::new();

            self.insert_symbol(var_id, sym);
            self.stack.type_scope_mut().enter(name, sym);
        }

        self.visit_type(*ty, tree)?;

        // Exit type parameter scope (in reverse order)
        for var in vars.iter().rev() {
            let name = var.get(tree).0.into();
            self.stack.type_scope_mut().exit(&name);
        }

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
        } else if let Some(type_sym) = self.stack.shape().get_type(name) {
            // Found a type binding in the current module scope
            // which was defined before this type path (no forward reference)

            self.insert_symbol(id, ResolvedType::Reference(type_sym));

            // Type Path's can occur in both type binds and type annotations.
            // Only in the former case we need to add a dependency.
            if let Some(current_sym) = self.current_type_bind_sym {
                // Add dependency from the current type bind to this type
                self.stack
                    .type_graph_mut()
                    .add_dependency(current_sym, type_sym);
            }

            ControlFlow::Continue(())
        } else if let Some(type_sym) = self.stack.type_scope().get(&name) {
            // Local quantifier will not create type bind cycle either
            self.insert_symbol(id, ResolvedType::Reference(*type_sym));
            ControlFlow::Continue(())
        } else if let Some(builtin) = self.interner.get(name.0).and_then(BuiltinType::from_name) {
            // This is a builtin type - resolve immediately, no dependencies needed
            self.insert_symbol(id, ResolvedType::Builtin(builtin));
            ControlFlow::Continue(())
        }
        // Either a forward reference or not found in the current module scope
        else if let Some(current_sym) = self.current_type_bind_sym {
            let ref_ = TypeBindConst::new(name, id, current_sym, loc);
            self.stack.cons_mut().insert_type_bind(ref_);

            ControlFlow::Continue(())
        } else {
            let ref_ = TypeConst::new(name, id, loc);
            self.stack.cons_mut().insert_type(ref_);

            ControlFlow::Continue(())
        }
    }

    fn visit_effect_type_bind(
        &mut self,
        id: Id<node::EffectTypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::EffectTypeBind { vis, name, ty } = *tree.node(id);

        let name = *name.get(tree);
        let vis = *vis.get(tree);
        let loc = self.span(id);

        let sym = EffectSym::new();
        self.current_effect_type_bind_sym = Some(sym);
        self.insert_symbol(id, sym);
        self.stack.effect_graph_mut().add_node(sym);

        // Register the type binding in the current scope
        if let Err(e) = self
            .stack
            .insert_effect(name, sym, Def::bound(id, vis, loc))
        {
            self.report.add_diagnostic(e.into());
        }

        self.visit_effect_row_type(ty, tree)?;

        self.current_effect_type_bind_sym = None;

        ControlFlow::Continue(())
    }

    fn visit_qualified_effect_type(
        &mut self,
        id: Id<node::QualifiedEffectType>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::QualifiedEffectType { path, ty } = *tree.node(id);

        let name = *ty.get(tree);
        let loc = self.span(id);

        if let Some(path) = path {
            // Just visit the module path if it exists
            self.visit_module_path(path, tree)
        } else if let Some(type_sym) = self.stack.shape().get_effect(name) {
            // Found a effect type binding in the current module scope
            // which was defined before this type path (no forward reference)

            self.insert_symbol(id, ResolvedEffect::Reference(type_sym));

            // Qualified effect types can occur in both effect type binds and effect type annotations.
            // Only in the former case we need to add a dependency.
            if let Some(current_sym) = self.current_effect_type_bind_sym {
                // Add dependency from the current type bind to this type
                self.stack
                    .effect_graph_mut()
                    .add_dependency(current_sym, type_sym);
            }

            ControlFlow::Continue(())
        } else if let Some(builtin) = self.interner.get(name.0).and_then(BuiltinEffect::from_name) {
            // This is a builtin effect type - resolve immediately, no dependencies needed
            self.insert_symbol(id, ResolvedEffect::Builtin(builtin));
            ControlFlow::Continue(())
        }
        // Either a forward reference or not found in the current module scope
        else if let Some(current_sym) = self.current_effect_type_bind_sym {
            let ref_ = EffectBindConst::new(name, id, current_sym, loc);
            self.stack.cons_mut().insert_effect_bind(ref_);

            ControlFlow::Continue(())
        } else {
            let ref_ = EffectConst::new(name, id, loc);
            self.stack.cons_mut().insert_effect(ref_);

            ControlFlow::Continue(())
        }
    }
}
