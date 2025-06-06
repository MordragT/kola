use std::ops::ControlFlow;

use kola_span::{Diagnostic, Loc};
use kola_syntax::loc::LocPhase;
use kola_tree::{node::Vis, prelude::*};
use kola_utils::{interner::PathKey, tracker::Tracker};

use crate::{context::ResolveContext, scope::LexicalScope, symbol::LocalSymbol};

// TODO do not use PathKey here, but rather a module_symbol
// Also I can probably do topological sort of the modules to get the list of module_symbols.
// I don't think it is necessary to define values in a specific order,
// but I will need it for type checking later on so might as well do it now.
pub fn define_value(path_key: PathKey, ctx: &mut ResolveContext) {
    let tree = ctx.forest.tree(path_key);

    let mut definer = ValueDefiner::new(path_key, ctx);

    // TODO If I do not use PathKey's I shouldn't visit the whole tree, but the module root instead.
    match tree.root_id().visit_by(&mut definer, &*tree) {
        ControlFlow::Continue(()) => (),
        ControlFlow::Break(_) => unreachable!(),
    }
}

pub struct ValueDefiner<'a> {
    pub path_key: PathKey,
    pub scope: LexicalScope,
    pub ctx: &'a mut ResolveContext,
}

impl<'a> ValueDefiner<'a> {
    pub fn new(path_key: PathKey, ctx: &'a mut ResolveContext) -> Self {
        Self {
            path_key,
            scope: LexicalScope::new(),
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

impl<'a, T> Visitor<T> for ValueDefiner<'a>
where
    T: TreeView,
{
    type BreakValue = !;

    fn visit_let_expr(&mut self, id: Id<node::LetExpr>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let node::LetExpr {
            name,
            value,
            inside,
        } = *tree.node(id);

        let name = tree.node(name).0;

        LocalSymbol::enter();
        self.walk_expr(value, tree)?;
        LocalSymbol::exit();

        let sym = LocalSymbol::new();

        self.ctx
            .symbol_table
            .insert_local_bind(self.path_key, id, sym);

        self.scope.insert(name, sym);
        self.walk_expr(inside, tree)?;
        self.scope.remove(&name);

        ControlFlow::Continue(())
    }

    fn visit_lambda_expr(
        &mut self,
        id: Id<node::LambdaExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::LambdaExpr { param, body } = *tree.node(id);

        let name = tree.node(param).0;

        let sym = LocalSymbol::new();

        self.ctx
            .symbol_table
            .insert_local_bind(self.path_key, id, sym);

        self.scope.insert(name, sym);
        self.walk_expr(body, tree)?;
        self.scope.remove(&name);

        ControlFlow::Continue(())
    }

    fn visit_path_expr(
        &mut self,
        id: Id<node::PathExpr>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::PathExpr {
            path,
            binding,
            select,
        } = tree.node(id);

        let binding = tree.node(*binding).0;

        if let Some(path) = path {
            let module_sym = self
                .ctx
                .symbol_table
                .lookup_path_sym(self.path_key, *path)
                .expect("Path should have been resolved before value definition");

            let module_id = self.ctx.symbol_table.lookup_module_id(module_sym).unwrap();
            let module_span = self.ctx.topography.span(module_id.0, module_id.1);

            let scope = self
                .ctx
                .module_scopes
                .get(&module_sym)
                .expect("Module scope should exist");

            let Some(bind) = scope.get_value(binding) else {
                self.ctx.report.add_diagnostic(
                    Diagnostic::error(self.span(*path), "Cannot find value binding")
                        .with_trace([("In this module".to_owned(), module_span)]),
                );
                return ControlFlow::Continue(());
            };

            if bind.vis != Vis::Export {
                self.ctx.report.add_diagnostic(
                    Diagnostic::error(self.span(*path), "Cannot access non-exported value binding")
                        .with_trace([("At this binding".to_owned(), bind.loc)]),
                );
                return ControlFlow::Continue(());
            }

            let value_sym = self
                .ctx
                .symbol_table
                .lookup_value_sym(module_id.0, bind.node_id)
                .expect("Value symbol should exist");

            // TODO okay now I have resolve the binding, and I know it is a value symbol,
            // of another module. Now just insert it into the symbol table ??
            // self.ctx.symbol_table.insert_value_path(
            //     self.path_key,
            //     *path,
            //     value_sym,
            // );
            todo!()
        } else if let Some(value_sym) = self.scope.get(&binding) {
            // TODO okay now just directly insert the symbol into the symbol table ??
            // self.ctx.symbol_table.insert_value_path(
            //     self.path_key,
            //     *path,
            //     value_sym,
            // );
            todo!()
        } else if let Some(info) = self.current_module().get_value(binding) {
            // TODO Or should I just insert every top-level binding into the lexical scope??
            todo!()
        } else {
            todo!()
        }

        todo!()
    }
}
