use log::debug;
use owo_colors::OwoColorize;
use std::ops::{ControlFlow, Deref};

use kola_print::prelude::*;
use kola_span::{IntoDiagnostic, Loc};
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_utils::interner::PathKey;

use crate::{
    context::ResolveContext,
    module::{
        ModuleBindInfo, ModuleKey, ModuleScope, TypeBindInfo, UnresolvedModuleScope, ValueBindInfo,
    },
};

#[derive(Debug, Clone, Default)]
pub struct ScopeStack {
    pub reverse: Vec<UnresolvedModuleScope>,
    pub scopes: Vec<UnresolvedModuleScope>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            reverse: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn start(&mut self, scope: UnresolvedModuleScope) {
        self.scopes.push(scope);
    }

    pub fn finish(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            // Store the scope in reverse order for later use
            self.reverse.push(scope);
        }
    }

    pub fn current(&self) -> Option<&UnresolvedModuleScope> {
        self.scopes.last()
    }

    pub fn current_mut(&mut self) -> &mut UnresolvedModuleScope {
        self.scopes.last_mut().unwrap()
    }
}

pub fn declare(path_key: PathKey, ctx: &mut ResolveContext) -> ModuleKey {
    let mut scopes = ScopeStack::new();

    let tree = ctx.forest.tree(path_key);

    // Create a visitor to walk the tree and collect declarations
    let mut declarer = Declarer::new(path_key, &mut scopes, ctx);

    match tree.root_id().visit_by(&mut declarer, tree.deref()) {
        ControlFlow::Continue(()) => (),
        ControlFlow::Break(_) => unreachable!(),
    }

    // After visiting the tree, we need to finalize the declaration
    let module_key = scopes.reverse.last().unwrap().module_key();

    for scope in scopes.reverse {
        ctx.unresolved.insert(scope.module_key(), scope);
    }

    module_key
}

pub struct Declarer<'a> {
    pub path_key: PathKey,
    pub scopes: &'a mut ScopeStack,
    pub ctx: &'a mut ResolveContext,
}

impl<'a> Declarer<'a> {
    pub fn new(path_key: PathKey, scopes: &'a mut ScopeStack, ctx: &'a mut ResolveContext) -> Self {
        Self {
            path_key,
            scopes,
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
        let module_key = ModuleKey::new();

        // Store the new module in our modules mapping
        self.ctx.mapping.insert(module_key, self.path_key, id);

        // Add dependency from current module to this new module
        if let Some(current_scope) = self.scopes.current() {
            self.ctx
                .dependencies
                .add_dependency(current_scope.module_key(), module_key);
        }

        // Create a new scope for this module
        self.scopes.start(ModuleScope::new(module_key));

        // Walk through children nodes
        self.walk_module(id, tree)?;

        // Pop the scope now that we're done with this module
        self.scopes.finish();

        ControlFlow::Continue(())
    }

    fn visit_module_import(
        &mut self,
        id: Id<node::ModuleImport>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
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
                    // Mock key for now
                    let module_key = ModuleKey::new();
                    let current_module_key = self.scopes.current().unwrap().module_key();

                    self.ctx
                        .dependencies
                        .add_dependency(current_module_key, module_key);
                    self.scopes.current_mut().insert_import(id, module_key);
                    // Mock end
                    // I am doing this because I am not fatally returning here,
                    // so that later on the module key can be resolved even if it is faulty itsself.
                    // Also doing the same for every other `return`

                    return ControlFlow::Continue(());
                }
            };

        let (path_key, source) = match self.ctx.source_manager.fetch(path.as_path(), &self.ctx.io) {
            Ok(tuple) => tuple,
            Err(e) => {
                self.ctx
                    .report
                    .add_diagnostic(e.into_diagnostic(self.span(id)).with_help("fetch"));

                // Mock key for now
                let module_key = ModuleKey::new();
                let current_module_key = self.scopes.current().unwrap().module_key();

                self.ctx
                    .dependencies
                    .add_dependency(current_module_key, module_key);
                self.scopes.current_mut().insert_import(id, module_key);

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
            // Mock key for now
            let module_key = ModuleKey::new();
            let current_module_key = self.scopes.current().unwrap().module_key();

            self.ctx
                .dependencies
                .add_dependency(current_module_key, module_key);
            self.scopes.current_mut().insert_import(id, module_key);

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
            // Mock key for now
            let module_key = ModuleKey::new();
            let current_module_key = self.scopes.current().unwrap().module_key();

            self.ctx
                .dependencies
                .add_dependency(current_module_key, module_key);
            self.scopes.current_mut().insert_import(id, module_key);

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

        let module_key = declare(path_key, &mut self.ctx);
        let current_module_key = self.scopes.current().unwrap().module_key();

        // Add dependency from current module to this new module
        self.ctx
            .dependencies
            .add_dependency(current_module_key, module_key);
        self.scopes.current_mut().insert_import(id, module_key);

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

        // Register the module binding in the current scope
        if let Err(e) = self
            .scopes
            .current_mut()
            .insert_module(name.0, ModuleBindInfo::new(id, value, span, *vis))
        {
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
        if let Err(e) = self
            .scopes
            .current_mut()
            .insert_value(name.0, ValueBindInfo::new(id, value, span, *vis))
        {
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
        if let Err(e) = self
            .scopes
            .current_mut()
            .insert_type(name.0, TypeBindInfo::new(id, ty, span))
        {
            self.ctx.report.add_diagnostic(e.into());
        }

        // self.walk_type_bind(id, tree)
        ControlFlow::Continue(()) // nothing to do here anyways
    }
}
