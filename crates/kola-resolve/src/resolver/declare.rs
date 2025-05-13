use kola_print::prelude::*;
use kola_span::IntoDiagnostic;
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_utils::{interner::PathKey, io::FileSystem};
use log::debug;
use owo_colors::OwoColorize;
use std::{
    ops::{ControlFlow, Deref},
    rc::Rc,
};

use crate::{
    forest::Forest,
    module::{ModuleKey, ModuleScope, ScopeStack},
    resolver::Resolver,
};

#[derive(Debug, Clone)]
pub struct Declare {
    pub path_key: PathKey,
    pub scopes: ScopeStack,
}

impl Declare {
    pub fn new(path_key: PathKey) -> Self {
        Self {
            path_key,
            scopes: ScopeStack::new(),
        }
    }

    pub fn declare<Io>(mut self, forest: &mut Forest<Io>) -> ModuleKey
    where
        Io: FileSystem,
    {
        let tree = forest.tree(self.path_key);

        // Create a visitor to walk the tree and collect declarations
        let mut declarer = Declarer {
            forest,
            state: &mut self,
        };

        match tree.root_id().visit_by(&mut declarer, tree.deref()) {
            ControlFlow::Continue(()) => (),
            ControlFlow::Break(_) => unreachable!(),
        }

        // After visiting the tree, we need to finalize the declaration
        let module_key = self.scopes.reverse.last().unwrap().module_key;

        for scope in self.scopes.reverse {
            forest.scopes.insert(scope.module_key, scope);
        }

        module_key
    }
}

pub struct Declarer<'a, Io> {
    pub forest: &'a mut Forest<Io>,
    pub state: &'a mut Declare,
}

impl<'a, T, Io> Visitor<T> for Declarer<'a, Io>
where
    T: TreeView,
    Io: FileSystem,
{
    type BreakValue = !;

    fn visit_module(&mut self, id: Id<node::Module>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let module_key = ModuleKey::new();

        // Store the new module in our modules mapping
        self.forest
            .mappings
            .insert(module_key, (self.state.path_key, id));

        // Add dependency from current module to this new module
        if let Some(current_scope) = self.state.scopes.current() {
            self.forest
                .dependencies
                .add_dependency(current_scope.module_key, module_key);
        }

        // Create a new scope for this module
        self.state.scopes.push(ModuleScope::new(module_key));

        // Walk through children nodes
        self.walk_module(id, tree)?;

        // Pop the scope now that we're done with this module
        self.state.scopes.pop();

        ControlFlow::Continue(())
    }

    fn visit_module_import(
        &mut self,
        id: Id<node::ModuleImport>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let import = tree.node(id);
        let name = tree.node(import.0);

        let path = match self
            .forest
            .sources
            .resolve_import(self.state.path_key, name.0)
        {
            Ok(path) => path,
            Err(e) => {
                self.forest
                    .report
                    .add_diagnostic(e.into_diagnostic(self.forest.span(self.state.path_key, id)));
                return ControlFlow::Continue(());
            }
        };

        let (path_key, source) = match self.forest.sources.fetch(path.as_path()) {
            Ok(tuple) => tuple,
            Err(e) => {
                self.forest
                    .report
                    .add_diagnostic(e.into_diagnostic(self.forest.span(self.state.path_key, id)));
                return ControlFlow::Continue(());
            }
        };

        debug!(
            "{} {}\n{}",
            "Source".bold().bright_white(),
            &path,
            source.text()
        );

        let Some(tokens) = tokenize(path_key, source.text(), &mut self.forest.report) else {
            return ControlFlow::Continue(());
        };

        debug!(
            "{} {:?}\n{}",
            "Tokens".bold().bright_white(),
            &path,
            TokenPrinter(&tokens).render(PrintOptions::default())
        );

        let input = ParseInput::new(path_key, tokens);

        let ParseOutput { tree, spans, .. } = parse(input, &mut self.forest.report);

        let Some(tree) = tree else {
            return ControlFlow::Continue(());
        };

        self.forest.trees.insert(path_key, Rc::new(tree));
        self.forest.topography.insert(path_key, Rc::new(spans));

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

        let module_key = Resolver::declare(path_key, self.forest).state.module_key;
        let current_module_key = self.state.scopes.current().unwrap().module_key;

        // Add dependency from current module to this new module
        self.forest
            .dependencies
            .add_dependency(current_module_key, module_key);

        ControlFlow::Continue(())
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let bind = tree.node(id);
        let name = tree.node(bind.name);

        // Register the module binding in the current scope
        self.state.scopes.current_mut().modules.insert(name.0, id);

        self.walk_module_bind(id, tree)
    }

    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let bind = tree.node(id);
        let name = tree.node(bind.name);

        // Register the value binding in the current scope
        self.state.scopes.current_mut().values.insert(name.0, id);

        // self.walk_value_bind(id, tree)
        ControlFlow::Continue(()) // nothing to do here anyways
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let bind = tree.node(id);
        let name = tree.node(bind.name);

        // Register the type binding in the current scope
        self.state.scopes.current_mut().types.insert(name.0, id);

        // self.walk_type_bind(id, tree)
        ControlFlow::Continue(()) // nothing to do here anyways
    }
}
