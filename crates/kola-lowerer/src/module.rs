use kola_ir::{
    instr as ir,
    ir::{Ir, IrBuilder},
};
use kola_resolver::{
    forest::Forest,
    resolver::ValueOrders,
    scope::{ModuleScope, ModuleScopes},
    symbol::{ModuleSym, ValueSym},
};
use kola_tree::{node, tree::Tree};

use crate::{normalizer::Normalizer, symbol::SymbolEnv};

pub struct LoweredModule {
    pub sym: ModuleSym,
    pub ir: Ir,
}

pub fn lower_module(scope: &ModuleScope, value_order: &[ValueSym], tree: &Tree) -> LoweredModule {
    let mut symbols = SymbolEnv::new(&scope.resolved);
    let mut builder = IrBuilder::new();

    let hole = symbols.next();
    let arg = builder.add(ir::Atom::Symbol(hole));
    let mut next = builder.add(ir::Expr::Ret(ir::RetExpr { arg }));

    for &value_sym in value_order {
        let id = scope.defs[value_sym].id();

        let value_bind = id.get(tree);

        let normalizer = Normalizer::new(id, next, hole, &mut builder, symbols);
        next = normalizer.run(tree);
    }

    let ir = builder.finish(next);

    LoweredModule {
        sym: scope.info.sym,
        ir,
    }
}

pub fn lower(
    scopes: &ModuleScopes,
    value_orders: &ValueOrders,
    forest: &Forest,
) -> Vec<LoweredModule> {
    let mut lowered_modules = Vec::new();

    for (sym, scope) in scopes.iter() {
        let value_order = &value_orders[sym];
        let tree = &*forest[scope.info.source];

        let lowered_module = lower_module(scope, value_order, tree);
        lowered_modules.push(lowered_module);
    }

    lowered_modules
}
