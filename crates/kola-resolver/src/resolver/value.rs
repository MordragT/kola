use std::{ops::ControlFlow, rc::Rc};

use kola_span::{Diagnostic, Loc, Report};
use kola_syntax::loc::LocPhase;
use kola_tree::{node::Vis, prelude::*};

use crate::{
    QualId,
    forest::Forest,
    resolver::ModuleScopes,
    scope::{LexicalScope, ModuleScope},
    symbol::{ModuleSym, SymbolTable, ValueSym},
    topography::Topography,
};

pub fn resolve_values(
    module_symbols: &[ModuleSym],
    forest: &Forest,
    topography: &Topography,
    module_scopes: &ModuleScopes,
    report: &mut Report,
    symbol_table: &mut SymbolTable,
) {
    for module_sym in module_symbols {
        let scope = module_scopes
            .get(module_sym)
            .expect("Module scope should exist");

        resolve_values_in_module(
            Rc::clone(scope),
            forest,
            topography,
            module_scopes,
            report,
            symbol_table,
        );
    }
}

pub fn resolve_values_in_module(
    scope: Rc<ModuleScope>,
    forest: &Forest,
    topography: &Topography,
    module_scopes: &ModuleScopes,
    report: &mut Report,
    symbol_table: &mut SymbolTable,
) {
    let root_id = scope.node_id();

    let tree = forest.tree(scope.path_key());

    let mut definer = ValueResolver::new(scope, topography, module_scopes, report, symbol_table);

    // TODO If I do not use PathKey's I shouldn't visit the whole tree, but the module root instead.
    match root_id.visit_by(&mut definer, &*tree) {
        ControlFlow::Continue(()) => (),
        ControlFlow::Break(_) => unreachable!(),
    }
}

struct ValueResolver<'a> {
    module_scope: Rc<ModuleScope>,
    scope: LexicalScope,
    topography: &'a Topography,
    module_scopes: &'a ModuleScopes,
    report: &'a mut Report,
    symbol_table: &'a mut SymbolTable,
}

impl<'a> ValueResolver<'a> {
    fn new(
        module_scope: Rc<ModuleScope>,
        topography: &'a Topography,
        module_scopes: &'a ModuleScopes,
        report: &'a mut Report,
        symbol_table: &'a mut SymbolTable,
    ) -> Self {
        Self {
            module_scope,
            scope: LexicalScope::new(),
            topography,
            module_scopes,
            report,
            symbol_table,
        }
    }

    #[inline]
    pub fn span<T>(&self, id: Id<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        self.topography.span((self.module_scope.path_key(), id))
    }

    #[inline]
    pub fn qual<T>(&self, id: Id<T>) -> QualId<T> {
        (self.module_scope.path_key(), id)
    }
}

impl<'a, T> Visitor<T> for ValueResolver<'a>
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

        self.symbol_table.insert_let_expr(self.qual(id), sym);

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

        self.symbol_table.insert_lambda_expr(self.qual(id), sym);

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
        let node::PathExpr { path, binding, .. } = tree.node(id);

        let binding = tree.node(*binding).0;

        let qual_id = self.qual(id);

        if let Some(path) = path {
            let module_sym = self
                .symbol_table
                .lookup_module_path(self.qual(*path))
                .expect("Path should have been resolved before value definition");

            let scope = self
                .module_scopes
                .get(&module_sym)
                .expect("Module scope should exist");

            let Some(value_sym) = scope.values.lookup_sym(binding) else {
                self.report.add_diagnostic(
                    Diagnostic::error(self.span(*path), "Cannot find value binding")
                        .with_trace([("In this module".to_owned(), scope.loc)]),
                );
                return ControlFlow::Continue(());
            };

            let bind = scope.values[value_sym];

            if bind.vis != Vis::Export {
                self.report.add_diagnostic(
                    Diagnostic::error(self.span(*path), "Cannot access non-exported value binding")
                        .with_trace([("At this binding".to_owned(), bind.loc)]),
                );
                return ControlFlow::Continue(());
            }

            self.symbol_table.insert_path_expr(qual_id, value_sym);
        } else if let Some(value_sym) = self.scope.get(&binding) {
            self.symbol_table.insert_path_expr(qual_id, *value_sym);
        } else if let Some(value_sym) = self.module_scope.values.lookup_sym(binding) {
            let bind = self.module_scope.values[value_sym];

            if bind.vis != Vis::Export {
                self.report.add_diagnostic(
                    Diagnostic::error(self.span(id), "Cannot access non-exported value binding")
                        .with_trace([("At this binding".to_owned(), bind.loc)]),
                );
                return ControlFlow::Continue(());
            }

            self.symbol_table.insert_path_expr(qual_id, value_sym);
        } else {
            self.report.add_diagnostic(
                Diagnostic::error(self.span(id), "Cannot find value binding")
                    .with_trace([("In this module".to_owned(), self.module_scope.loc)]),
            );

            return ControlFlow::Continue(());
        }

        ControlFlow::Continue(())
    }
}
