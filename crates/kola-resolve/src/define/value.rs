use std::{ops::ControlFlow, rc::Rc};

use kola_span::{Diagnostic, Loc};
use kola_syntax::loc::LocPhase;
use kola_tree::{node::Vis, prelude::*};

use crate::{
    QualId,
    context::ResolveContext,
    scope::{LexicalScope, ModuleScope},
    symbol::{ModuleSym, ValueSym},
};

// Also I can probably do topological sort of the modules to get the list of module_symbols.
// I don't think it is necessary to define values in a specific order,
// but I will need it for type checking later on so might as well do it now.
pub fn define_value(module_sym: ModuleSym, ctx: &mut ResolveContext) {
    let scope = ctx
        .module_scopes
        .get(&module_sym)
        .cloned()
        .expect("Module scope should exist");

    let root_id = scope.node_id();

    let tree = ctx.forest.tree(scope.path_key());

    let mut definer = ValueDefiner::new(scope, ctx);

    // TODO If I do not use PathKey's I shouldn't visit the whole tree, but the module root instead.
    match root_id.visit_by(&mut definer, &*tree) {
        ControlFlow::Continue(()) => (),
        ControlFlow::Break(_) => unreachable!(),
    }
}

pub struct ValueDefiner<'a> {
    pub module_scope: Rc<ModuleScope>,
    pub scope: LexicalScope,
    pub ctx: &'a mut ResolveContext,
}

impl<'a> ValueDefiner<'a> {
    pub fn new(module_scope: Rc<ModuleScope>, ctx: &'a mut ResolveContext) -> Self {
        Self {
            module_scope,
            scope: LexicalScope::new(),
            ctx,
        }
    }

    #[inline]
    pub fn span<T>(&self, id: Id<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        self.ctx.topography.span((self.module_scope.path_key(), id))
    }

    #[inline]
    pub fn qual<T>(&self, id: Id<T>) -> QualId<T> {
        (self.module_scope.path_key(), id)
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

        ValueSym::enter();
        self.walk_expr(value, tree)?;
        ValueSym::exit();

        let sym = ValueSym::new();

        self.ctx.lookup_table.insert_let_expr(self.qual(id), sym);

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

        let sym = ValueSym::new();

        self.ctx.lookup_table.insert_lambda_expr(self.qual(id), sym);

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

        let qual_id = self.qual(id);

        if let Some(path) = path {
            let module_sym = self
                .ctx
                .lookup_table
                .lookup_module_path(self.qual(*path))
                .expect("Path should have been resolved before value definition");

            dbg!(
                tree.node(*path)
                    .0
                    .iter()
                    .map(|id| self.ctx.interner.get(id.get(tree).0))
                    .collect::<Vec<_>>()
            );

            let scope = self
                .ctx
                .unresolved_scopes
                .get(&module_sym)
                .expect("Module scope should exist");

            let Some(bind) = scope.get_value(binding) else {
                self.ctx.report.add_diagnostic(
                    Diagnostic::error(self.span(*path), "Cannot find value binding")
                        .with_trace([("In this module".to_owned(), scope.loc)]),
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
                .lookup_table
                .lookup_value_bind(bind.id) // TODO replace Id with QualId in BindInfo ? or just QualName ??
                .expect("Value symbol should exist");

            self.ctx.lookup_table.insert_path_expr(qual_id, value_sym);
        } else if let Some(value_sym) = self.scope.get(&binding) {
            self.ctx.lookup_table.insert_path_expr(qual_id, *value_sym);
        } else if let Some(bind) = self.module_scope.get_value(binding) {
            if bind.vis != Vis::Export {
                self.ctx.report.add_diagnostic(
                    Diagnostic::error(self.span(id), "Cannot access non-exported value binding")
                        .with_trace([("At this binding".to_owned(), bind.loc)]),
                );
                return ControlFlow::Continue(());
            }

            let value_sym = self
                .ctx
                .lookup_table
                .lookup_value_bind(bind.id) // TODO replace Id with QualId in BindInfo ? or just QualName ??
                .expect("Value symbol should exist");

            self.ctx.lookup_table.insert_path_expr(qual_id, value_sym);
        } else {
            self.ctx.report.add_diagnostic(
                Diagnostic::error(self.span(id), "Cannot find value binding")
                    .with_trace([("In this module".to_owned(), self.module_scope.loc)]),
            );

            return ControlFlow::Continue(());
        }

        ControlFlow::Continue(())
    }
}
