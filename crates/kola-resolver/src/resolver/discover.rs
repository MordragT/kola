use camino::Utf8Path;
use log::debug;
use owo_colors::OwoColorize;
use std::{io, ops::ControlFlow};

use kola_print::prelude::*;
use kola_span::{IntoDiagnostic, Loc, Report, SourceId, SourceManager};
use kola_syntax::prelude::*;
use kola_tree::{node::Vis, prelude::*};
use kola_utils::{interner::StrInterner, io::FileSystem};

use crate::{
    GlobalId,
    defs::Def,
    forest::Forest,
    info::ModuleInfo,
    phase::ResolvePhase,
    prelude::Topography,
    refs::{ModuleRef, TypeBindRef, TypeRef, ValueRef},
    scope::{ModuleScope, ModuleScopeStack},
    symbol::{ModuleSym, TypeSym, ValueSym},
};

use super::ModuleGraph;

#[derive(Debug, Clone, Default)]
pub struct DiscoverOutput {
    pub source_manager: SourceManager,
    pub forest: Forest,
    pub topography: Topography,
    pub module_graph: ModuleGraph,
    pub module_scopes: Vec<ModuleScope>,
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

    let mut module_graph = ModuleGraph::new();
    let mut module_scopes = Vec::new();
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
        &mut entry_points,
        print_options,
    );

    Ok(DiscoverOutput {
        source_manager,
        forest,
        topography,
        module_graph,
        module_scopes,
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
        entry_points,
        print_options,
    );

    match tree.root_id().visit_by(&mut discoverer, &*tree) {
        ControlFlow::Continue(()) => (),
        ControlFlow::Break(_) => unreachable!(),
    }

    let tracker = discoverer.stack;
    module_scopes.append(&mut tracker.into_completed());
}

struct Discoverer<'a> {
    source_id: SourceId,
    current_module_bind_sym: Option<ModuleSym>,
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
        entry_points: &'a mut Vec<ValueSym>,
        print_options: PrintOptions,
    ) -> Self {
        Self {
            source_id,
            current_module_bind_sym: Some(module_sym),
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
        self.stack.finish();

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

        if let Some(module_sym) = self.current_module_bind_sym.take() {
            // If this is called from a module bind, we can insert the module path
            // module a = b::c

            self.insert_symbol(id, module_sym);
            self.stack.refs_mut().insert_module_bind(module_sym, path);
        } else {
            // If we don't have a current module bind, we need to create a reference
            // module::record.field

            let current_sym = self.stack.info().sym;
            let ref_ = ModuleRef::new(path, id, current_sym, self.span(id));

            self.stack.refs_mut().insert_module(ref_);
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
            ty,
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

        if let Some(ty) = ty {
            // If there is a type annotation, we need to visit it
            self.visit_type(ty, tree)?;
        }

        self.visit_expr(value, tree)?;

        self.current_value_bind_sym = None;

        ControlFlow::Continue(())
    }

    fn visit_let_expr(&mut self, id: Id<node::LetExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let node::LetExpr {
            name,
            value,
            inside,
        } = *tree.node(id);

        let name = *tree.node(name);

        ValueSym::enter();
        self.walk_expr(value, tree)?;
        ValueSym::exit();

        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        self.stack.value_scope_mut().enter(name, sym);
        self.walk_expr(inside, tree)?;
        self.stack.value_scope_mut().exit(&name);

        ControlFlow::Continue(())
    }

    fn visit_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr { param, body } = *tree.node(id);

        let name = *tree.node(param);

        let sym = ValueSym::new();
        self.insert_symbol(id, sym);

        self.stack.value_scope_mut().enter(name, sym);
        self.walk_expr(body, tree)?;
        self.stack.value_scope_mut().exit(&name);

        ControlFlow::Continue(())
    }

    fn visit_path_expr(
        &mut self,
        id: Id<node::PathExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::PathExpr { path, binding, .. } = tree.node(id);

        let name = *tree.node(*binding);

        if let Some(path) = path {
            // Just visit the module path if it exists
            self.visit_module_path(*path, tree)
        } else if let Some(value_sym) = self.stack.value_scope().get(&name) {
            // Local binding will not create value bind cycle either
            self.insert_symbol(id, *value_sym);
            ControlFlow::Continue(())
        } else if let Some(value_sym) = self.stack.shape().get_value(name) {
            // Found a value binding in the current module scope
            // which was defined before this path expression (no forward reference)

            self.insert_symbol(id, value_sym);

            let current_sym = self.current_value_bind_sym.unwrap();
            self.stack
                .value_graph_mut()
                .add_dependency(current_sym, value_sym);

            ControlFlow::Continue(())
        } else {
            // Either a forward reference or not found in the current module scope

            let current_sym = self.current_value_bind_sym.unwrap();
            let ref_ = ValueRef::new(name, id, current_sym, self.span(id));

            self.stack.refs_mut().insert_value(ref_);

            ControlFlow::Continue(())
        }
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind { name, ty } = *tree.node(id);

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

        self.visit_type(ty, tree)?; // walk to discover module paths in type expressions

        self.current_type_bind_sym = None;

        ControlFlow::Continue(())
    }

    fn visit_type(&mut self, id: Id<node::Type>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let node::Type { vars, ty } = id.get(tree);

        // Enter type parameter scope
        for &var_id in vars {
            let name = var_id.get(tree).0.into();
            let sym = TypeSym::new();

            self.insert_symbol(var_id, sym);
            self.stack.type_scope_mut().enter(name, sym);
        }

        self.visit_type_expr(*ty, tree)?;

        // Exit type parameter scope (in reverse order)
        for var in vars.iter().rev() {
            let name = var.get(tree).0.into();
            self.stack.type_scope_mut().exit(&name);
        }

        ControlFlow::Continue(())
    }

    fn visit_type_path(
        &mut self,
        id: Id<node::TypePath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypePath { path, ty } = *tree.node(id);

        let name = *ty.get(tree);

        let loc = self.span(id);

        if let Some(path) = path {
            // Just visit the module path if it exists
            self.visit_module_path(path, tree)
        } else if let Some(type_sym) = self.stack.shape().get_type(name) {
            // Found a type binding in the current module scope
            // which was defined before this type path (no forward reference)

            self.insert_symbol(id, type_sym);

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
            self.insert_symbol(id, *type_sym);
            ControlFlow::Continue(())
        } else if self.interner.get(name.0).is_some_and(is_builtin_type) {
            ControlFlow::Continue(())
        } else
        // Either a forward reference or not found in the current module scope
        if let Some(current_sym) = self.current_type_bind_sym {
            let ref_ = TypeBindRef::new(name, id, current_sym, loc);
            self.stack.refs_mut().insert_type_bind(ref_);

            ControlFlow::Continue(())
        } else {
            let ref_ = TypeRef::new(name, id, loc);
            self.stack.refs_mut().insert_type(ref_);

            ControlFlow::Continue(())
        }
    }
}

fn is_builtin_type(name: &str) -> bool {
    matches!(name, "Bool" | "Num" | "Str" | "Char")
}
