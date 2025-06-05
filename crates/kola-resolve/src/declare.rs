use log::debug;
use owo_colors::OwoColorize;
use std::ops::{ControlFlow, Deref};

use kola_print::prelude::*;
use kola_span::{IntoDiagnostic, Loc};
use kola_syntax::prelude::*;
use kola_tree::{node::Vis, prelude::*};
use kola_utils::{interner::PathKey, tracker::Tracker};

use crate::{
    context::ResolveContext,
    info::BindInfo,
    scope::ModuleScope,
    symbol::{ModuleSymbol, TypeSymbol, ValueSymbol},
};

pub fn declare(path_key: PathKey, module_sym: ModuleSymbol, ctx: &mut ResolveContext) {
    let mut tracker = Tracker::new();

    let tree = ctx.forest.tree(path_key);

    // Create a visitor to walk the tree and collect declarations
    let mut declarer = Declarer::new(path_key, module_sym, &mut tracker, ctx);

    match tree.root_id().visit_by(&mut declarer, tree.deref()) {
        ControlFlow::Continue(()) => (),
        ControlFlow::Break(_) => unreachable!(),
    }

    let scopes = tracker.into_completed();

    // After visiting the tree, we need to finalize the declaration
    for scope in scopes {
        let sym = scope.symbol();

        ctx.todo.insert(sym);
        ctx.module_scopes.insert(sym, scope);
    }
}

pub struct Declarer<'a> {
    pub path_key: PathKey,
    pub symbols: Vec<ModuleSymbol>,
    pub tracker: &'a mut Tracker<ModuleScope>,
    pub ctx: &'a mut ResolveContext,
}

impl<'a> Declarer<'a> {
    pub fn new(
        path_key: PathKey,
        symbol: ModuleSymbol, // Start symbol ??
        tracker: &'a mut Tracker<ModuleScope>,
        ctx: &'a mut ResolveContext,
    ) -> Self {
        Self {
            path_key,
            symbols: vec![symbol],
            tracker,
            ctx,
        }
    }

    pub fn span<T>(&self, id: Id<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        self.ctx.topography.span(self.path_key, id)
    }
}

impl<'a, T> Visitor<T> for Declarer<'a>
where
    T: TreeView,
{
    type BreakValue = !;

    fn visit_module(&mut self, id: Id<node::Module>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let module_sym = self.symbols.pop().unwrap();

        self.ctx
            .symbol_table
            .insert_inline(self.path_key, id, module_sym);

        // Add dependency from current module to this new module
        if let Some(current_scope) = self.tracker.current() {
            self.ctx
                .module_graph
                .add_dependency(current_scope.symbol(), module_sym);
        }

        // Create a new scope for this module
        self.tracker
            .start(ModuleScope::new(module_sym, self.span(id)));

        // Walk through children nodes
        self.walk_module(id, tree)?;

        // Pop the scope now that we're done with this module
        self.tracker.finish();

        ControlFlow::Continue(())
    }

    fn visit_module_import(
        &mut self,
        id: Id<node::ModuleImport>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let module_sym = self.symbols.pop().unwrap();

        let import = tree.node(id);
        let name = tree.node(import.0);

        let path =
            match self
                .ctx
                .source_manager
                .resolve_import(self.path_key, name.0, &self.ctx.interner)
            {
                Ok(path) => path,
                Err(e) => {
                    self.ctx.report.add_diagnostic(
                        e.into_diagnostic(self.span(id)).with_help("resolve_import"),
                    );

                    // TODO make this more elegant
                    // I am doing this because I am not fatally returning here.
                    // Also doing the same for every other `return`.
                    let current_module_sym = self.tracker.current().unwrap().symbol();
                    self.ctx
                        .module_graph
                        .add_dependency(current_module_sym, module_sym);
                    self.ctx
                        .symbol_table
                        .insert_import(self.path_key, id, module_sym);
                    return ControlFlow::Continue(());
                }
            };

        let (path_key, source) = match self.ctx.source_manager.fetch(path.as_path(), &self.ctx.io) {
            Ok(tuple) => tuple,
            Err(e) => {
                self.ctx
                    .report
                    .add_diagnostic(e.into_diagnostic(self.span(id)).with_help("fetch"));

                let current_module_sym = self.tracker.current().unwrap().symbol();

                self.ctx
                    .module_graph
                    .add_dependency(current_module_sym, module_sym);
                self.ctx
                    .symbol_table
                    .insert_import(self.path_key, id, module_sym);

                return ControlFlow::Continue(());
            }
        };

        debug!(
            "{} {}\n{}",
            "Source".bold().bright_white(),
            &path,
            source.text()
        );

        let input = LexInput::new(path_key, source.text());
        let Some(tokens) = tokenize(input, &mut self.ctx.report) else {
            let current_module_sym = self.tracker.current().unwrap().symbol();

            self.ctx
                .module_graph
                .add_dependency(current_module_sym, module_sym);
            self.ctx
                .symbol_table
                .insert_import(self.path_key, id, module_sym);

            return ControlFlow::Continue(());
        };

        debug!(
            "{} {:?}\n{}",
            "Tokens".bold().bright_white(),
            &path,
            TokenPrinter(&tokens).render(PrintOptions::default())
        );

        let input = ParseInput::new(path_key, tokens);

        let ParseOutput { tree, spans, .. } =
            parse(input, &mut self.ctx.interner, &mut self.ctx.report);

        let Some(tree) = tree else {
            let current_module_sym = self.tracker.current().unwrap().symbol();

            self.ctx
                .module_graph
                .add_dependency(current_module_sym, module_sym);
            self.ctx
                .symbol_table
                .insert_import(self.path_key, id, module_sym);

            return ControlFlow::Continue(());
        };

        self.ctx.forest.insert(path_key, tree);
        self.ctx.topography.insert(path_key, spans);

        // let interner = STR_INTERNER.read().unwrap();

        // debug!(
        //     "{} {:?}\n{}",
        //     "Untyped Abstract Syntax Tree".bold().bright_white(),
        //     &path,
        //     TreePrinter::new(tree.clone(), &interner)
        //         .with(LocDecorator(spans.clone()))
        //         .render(PrintOptions::default())
        // );

        // drop(interner);

        declare(path_key, module_sym, &mut self.ctx);

        let current_module_sym = self.tracker.current().unwrap().symbol();

        // Add dependency from current module to this new module
        self.ctx
            .module_graph
            .add_dependency(current_module_sym, module_sym);
        self.ctx
            .symbol_table
            .insert_import(self.path_key, id, module_sym);

        ControlFlow::Continue(())
    }

    fn visit_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let module_sym = self.symbols.pop().unwrap();

        let path = tree
            .node(id)
            .0
            .iter()
            .copied()
            .map(|id| tree.node(id).0)
            .collect::<Vec<_>>();

        self.ctx.module_paths.insert(module_sym, path);

        ControlFlow::Continue(())
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleBind {
            vis,
            name,
            ty,
            value,
        } = *tree.node(id);

        let name = tree.node(name);
        let vis = tree.node(vis);

        let span = self.span(id);

        let module_sym = ModuleSymbol::new();
        self.symbols.push(module_sym);

        // TODO is this right ?
        self.ctx
            .symbol_table
            .insert_module_bind(self.path_key, id, module_sym);

        // Register the module binding in the current scope
        if let Err(e) = self.tracker.current_mut().unwrap().insert_module(
            name.0,
            module_sym,
            BindInfo::new(id, span, *vis),
        ) {
            self.ctx.report.add_diagnostic(e.into());
        }

        self.walk_module_bind(id, tree)
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

        let name = tree.node(name);
        let vis = tree.node(vis);

        let span = self.span(id);

        // Register the value binding in the current scope
        if let Err(e) = self.tracker.current_mut().unwrap().insert_value(
            name.0,
            ValueSymbol::new(),
            BindInfo::new(id, span, *vis),
        ) {
            self.ctx.report.add_diagnostic(e.into());
        }

        // self.walk_value_bind(id, tree)
        ControlFlow::Continue(()) // nothing to do here anyways
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind { name, ty } = *tree.node(id);
        let name = tree.node(name);

        let span = self.span(id);

        // Register the type binding in the current scope
        if let Err(e) = self.tracker.current_mut().unwrap().insert_type(
            name.0,
            TypeSymbol::new(),
            BindInfo::new(id, span, Vis::Export),
        ) {
            self.ctx.report.add_diagnostic(e.into());
        }

        // self.walk_type_bind(id, tree)
        ControlFlow::Continue(()) // nothing to do here anyways
    }
}
