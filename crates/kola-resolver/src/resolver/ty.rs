use kola_span::{Diagnostic, Report};
use kola_tree::meta::MetaMapExt;
use log::debug;

use crate::{
    constraints::{TypeBindConst, TypeConst},
    phase::ResolvedType,
    scope::{ModuleScope, ModuleScopes},
};

use super::TypeOrders;

pub struct TypeResolution {
    pub type_orders: TypeOrders,
}

pub fn resolve_types(scopes: &mut ModuleScopes, report: &mut Report) -> TypeResolution {
    let mut type_orders = TypeOrders::new();

    for (sym, scope) in scopes {
        resolve_types_in_module(scope, report);

        match scope.type_graph.topological_sort() {
            Ok(order) => {
                type_orders.insert(*sym, order);
            }
            Err(cycle) => {
                report.add_diagnostic(
                    Diagnostic::error(scope.info.loc, cycle.to_string())
                        .with_help("Check for circular dependencies in type definitions."),
                );

                debug!("Cycle detected inside:\n{}", scope.type_graph.to_dot());
            }
        }
    }

    TypeResolution { type_orders }
}

fn resolve_types_in_module(scope: &mut ModuleScope, report: &mut Report) {
    // Resolve type references similar to value references
    for &TypeBindConst {
        name,
        id,
        source,
        loc,
    } in scope.cons.type_binds()
    {
        if let Some(target) = scope.shape.get_type(name) {
            scope.type_graph.add_dependency(source, target);
            scope
                .resolved
                .insert_meta(id, ResolvedType::Reference(target));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Type not found")
                    .with_help("Check that the type is defined in this module."),
            );
        }
    }

    for type_const in scope.cons.types() {
        match type_const {
            &TypeConst::Qualified { name, id, loc } => {
                if let Some(type_sym) = scope.shape.get_type(name) {
                    scope
                        .resolved
                        .insert_meta(id, ResolvedType::Reference(type_sym));
                } else {
                    report.add_diagnostic(
                        Diagnostic::error(loc, "Type not found in annotation")
                            .with_help("Check that the type is defined in this module."),
                    );
                }
            } // &TypeConst::TypeRep { name, id, loc } => {
              //     if let Some(type_sym) = scope.shape.get_type(name) {
              //         scope
              //             .resolved
              //             .insert_meta(id, ResolvedType::Reference(type_sym));
              //     } else {
              //         report.add_diagnostic(
              //             Diagnostic::error(loc, "Type not found in annotation")
              //                 .with_help("Check that the type is defined in this module."),
              //         );
              //     }
              // }
        }
    }
}
