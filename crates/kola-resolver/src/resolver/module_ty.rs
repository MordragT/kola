use kola_span::{Diagnostic, Report};
use kola_tree::meta::MetaMapExt;

use crate::{
    phase::ResolvedModuleType,
    refs::{ModuleTypeBindRef, ModuleTypeRef},
    resolver::ModuleTypeOrders,
    scope::{ModuleScope, ModuleScopes},
};

pub struct ModuleTypeResolution {
    pub module_type_orders: ModuleTypeOrders,
}

pub fn resolve_module_types(
    scopes: &mut ModuleScopes,
    report: &mut Report,
) -> ModuleTypeResolution {
    let mut module_type_orders = ModuleTypeOrders::new();

    for (sym, scope) in scopes {
        resolve_module_types_in_module(scope, report);

        match scope.module_type_graph.topological_sort() {
            Ok(order) => {
                module_type_orders.insert(*sym, order);
            }
            Err(_) => {
                report.add_diagnostic(
                    Diagnostic::error(scope.info.loc, "Cycle detected in module type graph")
                        .with_help("Check for circular dependencies in module type definitions."),
                );
            }
        }
    }

    ModuleTypeResolution { module_type_orders }
}

fn resolve_module_types_in_module(scope: &mut ModuleScope, report: &mut Report) {
    // Resolve module type bind references
    for &ModuleTypeBindRef {
        name,
        id,
        source,
        loc,
    } in scope.refs.module_type_binds()
    {
        if let Some(target) = scope.shape.get_module_type(name) {
            scope.module_type_graph.add_dependency(source, target);
            scope.resolved.insert_meta(id, ResolvedModuleType(target));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Module type not found")
                    .with_help("Check that the module type is defined in this module."),
            );
        }
    }

    // Resolve module type references
    for &ModuleTypeRef { name, id, loc } in scope.refs.module_types() {
        if let Some(module_type_sym) = scope.shape.get_module_type(name) {
            scope
                .resolved
                .insert_meta(id, ResolvedModuleType(module_type_sym));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Module type not found in annotation")
                    .with_help("Check that the module type is defined in this module."),
            );
        }
    }
}
