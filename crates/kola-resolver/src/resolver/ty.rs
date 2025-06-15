use kola_span::{Diagnostic, Report};
use kola_tree::meta::MetaMapExt;

use crate::{
    refs::TypeRef,
    scope::{ModuleScope, ModuleScopes},
    symbol::ModuleSym,
};

use super::TypeOrders;

pub struct TypeResolution {
    pub type_orders: TypeOrders,
}

pub fn resolve_types(
    symbols: &[ModuleSym],
    scopes: &mut ModuleScopes,
    report: &mut Report,
) -> TypeResolution {
    let mut type_orders = TypeOrders::new();

    for module_sym in symbols {
        let scope = &mut scopes[module_sym];

        resolve_types_in_module(scope, report);

        match scope.type_graph.topological_sort() {
            Ok(order) => {
                type_orders.insert(*module_sym, order);
            }
            Err(_) => {
                report.add_diagnostic(
                    Diagnostic::error(scope.info.loc, "Cycle detected in type graph")
                        .with_help("Check for circular dependencies in type definitions."),
                );
            }
        }
    }

    TypeResolution { type_orders }
}

fn resolve_types_in_module(scope: &mut ModuleScope, report: &mut Report) {
    // Resolve type references similar to value references
    for &TypeRef {
        name,
        id,
        source,
        loc,
    } in scope.refs.types()
    {
        if let Some(target) = scope.shape.get_type(name) {
            scope.type_graph.add_dependency(source, target);
            scope.resolved.insert_meta(id, target);
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Type not found")
                    .with_help("Check that the type is defined in this module."),
            );
        }
    }
}
