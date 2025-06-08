use log::debug;
use owo_colors::OwoColorize;
use std::{ops::ControlFlow, rc::Rc};

use kola_print::prelude::*;
use kola_span::{IntoDiagnostic, Loc};
use kola_syntax::prelude::*;
use kola_tree::{node::Vis, prelude::*};
use kola_utils::{interner::PathKey, tracker::Tracker};

use crate::{
    QualId,
    context::ResolveContext,
    info::{BindInfo, ModuleInfo},
    scope::ModuleScope,
    symbol::{ModuleSym, TypeSym, ValueSym},
};

pub fn declare(path_key: PathKey, module_sym: ModuleSym, ctx: &mut ResolveContext) {
    let mut tracker = Tracker::new();

    let tree = ctx.forest.tree(path_key);

    // Create a visitor to walk the tree and collect declarations
    let mut declarer = Declarer::new(path_key, module_sym, &mut tracker, ctx);

    match tree.root_id().visit_by(&mut declarer, &*tree) {
        ControlFlow::Continue(()) => (),
        ControlFlow::Break(_) => unreachable!(),
    }

    let scopes = tracker.into_completed();

    // After visiting the tree, we need to finalize the declaration
    for scope in scopes.into_iter().rev() {
        ctx.unresolved_scopes.insert(scope.symbol(), scope);
    }
}

pub struct Declarer<'a> {
    pub path_key: PathKey,
    pub current_sym: Option<ModuleSym>,
    pub current_bind: Option<Id<node::ModuleBind>>,
    pub tracker: &'a mut Tracker<ModuleScope>,
    pub ctx: &'a mut ResolveContext,
}

impl<'a> Declarer<'a> {
    pub fn new(
        path_key: PathKey,
        symbol: ModuleSym, // Start symbol ??
        tracker: &'a mut Tracker<ModuleScope>,
        ctx: &'a mut ResolveContext,
    ) -> Self {
        Self {
            path_key,
            current_sym: Some(symbol),
            current_bind: None,
            tracker,
            ctx,
        }
    }

    #[inline]
    pub fn span<T>(&self, id: Id<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        self.ctx.topography.span((self.path_key, id))
    }

    #[inline]
    pub fn qual<T>(&self, id: Id<T>) -> QualId<T> {
        (self.path_key, id)
    }

    #[inline]
    pub fn current_bind(&self) -> Option<QualId<node::ModuleBind>> {
        self.current_bind.map(|id| (self.path_key, id))
    }

    #[inline]
    pub fn current_module(&self) -> &ModuleScope {
        self.tracker.current().unwrap()
    }

    #[inline]
    pub fn current_module_mut(&mut self) -> &mut ModuleScope {
        self.tracker.current_mut().unwrap()
    }
}

impl<'a, T> Visitor<T> for Declarer<'a>
where
    T: TreeView,
{
    type BreakValue = !;

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
        let qual_id = self.qual(id);
        let module_sym = self.current_sym.unwrap_or_default();
        // self.current_sym = Some(module_sym);
        self.current_bind = Some(id);

        self.ctx
            .lookup_table
            .insert_module_bind(qual_id, module_sym);

        // TODO Hacky stuff here to adhere to the root handling
        // It would be far better to have some kind of root declare function as initializer
        // Or maybe insert a scope in the tracker before visiting the tree
        if self.current_sym.is_none() {
            // Register the module binding in the current scope
            if let Err(e) = self.current_module_mut().insert_module(
                name.0,
                module_sym,
                BindInfo::new(qual_id, span, *vis),
            ) {
                self.ctx.report.add_diagnostic(e.into());
            }
        }

        self.current_sym = Some(module_sym);

        self.walk_module_bind(id, tree)
    }

    fn visit_module(&mut self, id: Id<node::Module>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let module_sym = self.current_sym.take().unwrap_or_default();

        self.ctx
            .lookup_table
            .insert_module(self.qual(id), module_sym);

        // Add dependency from current module to this new module
        if let Some(current_scope) = self.tracker.current() {
            self.ctx
                .module_graph
                .add_dependency(current_scope.symbol(), module_sym);
        }

        // Create a new scope for this module
        self.tracker.start(ModuleScope::new(
            self.current_bind().unwrap(),
            module_sym,
            self.span(id),
        ));

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
        let module_sym = self.current_sym.take().unwrap();

        self.ctx
            .lookup_table
            .insert_module_import(self.qual(id), module_sym);

        let current_module_sym = self.current_module().symbol();

        self.ctx
            .module_graph
            .add_dependency(current_module_sym, module_sym);

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
                    return ControlFlow::Continue(());
                }
            };

        let (path_key, source) = match self.ctx.source_manager.fetch(path.as_path(), &self.ctx.io) {
            Ok(tuple) => tuple,
            Err(e) => {
                self.ctx
                    .report
                    .add_diagnostic(e.into_diagnostic(self.span(id)).with_help("fetch"));
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
            return ControlFlow::Continue(());
        };

        debug!(
            "{} {:?}\n{}",
            "Untyped Abstract Syntax Tree".bold().bright_white(),
            &path,
            TreePrinter::new(tree.clone(), self.ctx.interner.clone()) // TODO cloning the entire interner is not a good idea
                .with(LocDecorator(spans.clone()))
                .render(PrintOptions::default())
        );

        self.ctx.forest.insert(path_key, tree);
        self.ctx.topography.insert(path_key, spans);

        declare(path_key, module_sym, &mut self.ctx);

        ControlFlow::Continue(())
    }

    fn visit_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // This will create a new module symbol if not visited from a module bind
        let module_sym = self.current_sym.take().unwrap_or_default();

        self.ctx
            .lookup_table
            .insert_module_path(self.qual(id), module_sym);

        let path = tree
            .node(id)
            .0
            .iter()
            .copied()
            .map(|id| tree.node(id).0)
            .collect::<Vec<_>>();

        self.current_module_mut().insert_path(module_sym, path);

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

        let name = tree.node(name);
        let vis = tree.node(vis);

        let span = self.span(id);

        let qual_id = self.qual(id);

        let value_sym = ValueSym::new();

        self.ctx.lookup_table.insert_value_bind(qual_id, value_sym);

        // Register the value binding in the current scope
        if let Err(e) = self.current_module_mut().insert_value(
            name.0,
            value_sym,
            BindInfo::new(qual_id, span, *vis),
        ) {
            self.ctx.report.add_diagnostic(e.into());
        }

        self.walk_value_bind(id, tree) // walk to discover module paths in expressions
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind { name, ty } = *tree.node(id);
        let name = tree.node(name);

        let span = self.span(id);

        let qual_id = self.qual(id);

        let type_sym = TypeSym::new();

        self.ctx.lookup_table.insert_type_bind(qual_id, type_sym);

        // Register the type binding in the current scope
        if let Err(e) = self.current_module_mut().insert_type(
            name.0,
            type_sym,
            BindInfo::new(qual_id, span, Vis::Export),
        ) {
            self.ctx.report.add_diagnostic(e.into());
        }

        // self.walk_type_bind(id, tree) // TODO walk to discover module paths in type expressions
        ControlFlow::Continue(()) // nothing to do here at the moment
    }
}
