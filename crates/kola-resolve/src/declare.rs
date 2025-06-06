use log::debug;
use owo_colors::OwoColorize;
use std::{
    ops::{ControlFlow, Deref},
    rc::Rc,
};

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
        ctx.module_scopes.insert(scope.symbol(), Rc::new(scope));
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

    pub fn current_module(&self) -> &ModuleScope {
        self.tracker.current().unwrap()
    }

    pub fn current_module_mut(&mut self) -> &mut ModuleScope {
        self.tracker.current_mut().unwrap()
    }

    // pub fn current_lexical(&self) -> &LexicalScope {
    //     &self.tracker.current().unwrap().1
    // }

    // pub fn current_lexical_mut(&mut self) -> &mut LexicalScope {
    //     &mut self.tracker.current_mut().unwrap().1
    // }
}

impl<'a, T> Visitor<T> for Declarer<'a>
where
    T: TreeView,
{
    type BreakValue = !;

    // fn visit_let_expr(&mut self, id: Id<node::LetExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
    //     let node::LetExpr {
    //         name,
    //         value,
    //         inside,
    //     } = *tree.node(id);

    //     let name = tree.node(name).0;

    //     LocalSymbol::enter();
    //     self.walk_expr(value, tree)?;
    //     LocalSymbol::exit();

    //     let sym = LocalSymbol::new();

    //     self.ctx
    //         .symbol_table
    //         .insert_local_bind(self.path_key, id, sym);

    //     self.current_lexical_mut().insert(name, sym);

    //     self.walk_expr(inside, tree)
    // }

    // fn visit_path_expr(
    //     &mut self,
    //     id: Id<node::PathExpr>,
    //     tree: &T,
    // ) -> ControlFlow<Self::BreakValue> {
    //     let node::PathExpr {
    //         path,
    //         binding,
    //         select,
    //     } = tree.node(id);

    //     let binding = tree.node(*binding).0;

    //     if let Some(path) = path {
    //         // Cannot reuse the current visitor because that expects a module bind (look at the symbols pop)
    //         // Also trying to resolve at this point is not correct
    //         // It is possible to either defer this to a later phase,
    //         // or try to somehow link these module paths to the paths which are already introduced in module binds:
    //         // - e.g. through a BiMap of Vec<StrKey> to ModuleSymbol
    //         // But because the definer only runs over the module binds,
    //         // this could leave some module paths in path expressions unresolved.
    //         // So it might be the cleanest solution to just defer the resolution,
    //         // and create a new SideTable which stores the Vec<StrKey> for a new ModuleSymbol.
    //         // Another option is to later in the define stage once more go over all module paths
    //         // and actually check if they have been resolved
    //         todo!()
    //     }
    //     // Cannot resolve the binding at this point...
    //     // actually if there exists a module path,
    //     // then I do not need to resolve the binding,
    //     // or rather I do not need the LexicalScope to resolve it.
    //     // So I guess I could check if there is some module path,
    //     // and if not I can just use the LexicalScope and if there is I defer.
    //     else {
    //         if let Some(sym) = self.current_lexical().get(&binding) {
    //             todo!()
    //         }
    //         // Well I think I also need the current ModuleScope, so I definitely need to defer until later.
    //         // Also this could create a recursive call, which I want to prohibit.
    //         // So I guess I will need a similar mechanism to the `todo` and `active` sets in the context,
    //         // but for values instead of modules. A problem here could be that I actually have 2 different types
    //         // of symbols for values: LocalSymbol and ValueSymbol.
    //         else if let Some(info) = self.current_module().get_value(binding) {
    //             todo!()
    //         } else {
    //             todo!()
    //         }
    //     }
    //     todo!()
    // }

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
                    let current_module_sym = self.current_module().symbol();
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

                let current_module_sym = self.current_module().symbol();

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
            let current_module_sym = self.current_module().symbol();

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
            let current_module_sym = self.current_module().symbol();

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

        let current_module_sym = self.current_module().symbol();

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
        // This will create a new module symbol if not visited from a module bind
        let module_sym = match self.symbols.pop() {
            Some(sym) => sym,
            None => {
                // TODO if the symbol generation would be done centralized,
                // then I might be able to abstract this away,
                // so that I can just after the declare stage mark all as pending.
                let sym = ModuleSymbol::new();
                self.ctx.pending.insert(sym);
                sym
            }
        };

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
        self.ctx.pending.insert(module_sym);
        self.symbols.push(module_sym);

        // TODO is this right ?
        self.ctx
            .symbol_table
            .insert_module_bind(self.path_key, id, module_sym);

        // Register the module binding in the current scope
        if let Err(e) = self.current_module_mut().insert_module(
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
        if let Err(e) = self.current_module_mut().insert_value(
            name.0,
            ValueSymbol::new(),
            BindInfo::new(id, span, *vis),
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

        // Register the type binding in the current scope
        if let Err(e) = self.current_module_mut().insert_type(
            name.0,
            TypeSymbol::new(),
            BindInfo::new(id, span, Vis::Export),
        ) {
            self.ctx.report.add_diagnostic(e.into());
        }

        // self.walk_type_bind(id, tree) // TODO walk to discover module paths in type expressions
        ControlFlow::Continue(()) // nothing to do here at the moment
    }
}
