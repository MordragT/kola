use kola_print::prelude::*;
use kola_resolver::{
    forest::Forest,
    info::ModuleGraph,
    prelude::Topography,
    resolver::{TypeOrders, ValueOrders},
    scope::ModuleScopes,
};
use kola_span::{IntoDiagnostic, Issue, Report};
use kola_syntax::loc::LocDecorator;
use kola_tree::{
    meta::MetaView,
    print::{Decorators, TreePrinter},
};
use kola_utils::{fmt::StrInternerExt, interner::StrInterner};
use log::debug;

use crate::{
    env::TypeEnv,
    phase::{TypeAnnotations, TypedNodes},
    print::TypeDecorator,
    typer::Typer,
    types::ModuleType,
};

#[derive(Debug, Clone, Default)]
pub struct TypeCheckOutput {
    pub type_env: TypeEnv,
    pub type_annotations: TypeAnnotations,
}

pub fn type_check(
    forest: &Forest,
    topography: &Topography,
    module_graph: &ModuleGraph,
    module_scopes: &ModuleScopes,
    value_orders: &ValueOrders,
    type_orders: &TypeOrders,
    arena: &Bump,
    interner: &mut StrInterner,
    report: &mut Report,
    print_options: PrintOptions,
) -> TypeCheckOutput {
    let mut type_env = TypeEnv::new();
    let mut type_annotations = TypeAnnotations::new();

    let module_order = match module_graph.topological_sort() {
        Ok(order) => order,
        Err(_cycle) => {
            // TODO error reporting isn't great here,
            // but it is hard to know where the exact error is,
            // still should report the cycle.
            report.add_issue(Issue::error("Module cycle detected", 0));
            return TypeCheckOutput::default();
        }
    };

    for module_sym in module_order {
        let module_scope = module_scopes[&module_sym].clone();
        let info = module_scope.info;

        let tree = &*forest[info.source];
        let spans = topography[info.source].clone();
        let value_order = &value_orders[&module_sym];
        let type_order = &type_orders[&module_sym];
        let mut module_annotations = TypedNodes::new();

        for &type_sym in type_order {
            let id = module_scope.defs[type_sym].id();

            let typer = Typer::new(
                id,
                spans.clone(),
                &type_env,
                interner,
                &module_scope.resolved,
            );
            let typed_nodes = match typer.solve(tree, report) {
                Ok(typed_nodes) => typed_nodes,
                Err((errors, span)) => {
                    let diag = interner.display(&errors).into_diagnostic(span);
                    report.add_diagnostic(diag);
                    continue;
                }
            };

            type_env.insert_type(type_sym, typed_nodes.meta(id).clone());
            module_annotations.extend(typed_nodes);
        }

        if !report.is_empty() {
            // If there are errors in type checking types, break out early
            break;
        }

        for &value_sym in value_order {
            let id = module_scope.defs[value_sym].id();

            let typer = Typer::new(
                id,
                spans.clone(),
                &type_env,
                interner,
                &module_scope.resolved,
            );
            let typed_nodes = match typer.solve(tree, report) {
                Ok(typed_nodes) => typed_nodes,
                Err((errors, span)) => {
                    let diag = interner.display(&errors).into_diagnostic(span);
                    report.add_diagnostic(diag);
                    continue;
                }
            };

            type_env.insert_value(value_sym, typed_nodes.meta(id).clone());
            module_annotations.extend(typed_nodes);
        }

        if !report.is_empty() {
            // If there are errors in type checking values, break out early
            break;
        }

        let module_type = ModuleType::from(module_scope.shape.clone());
        type_env.insert_module(module_sym, module_type);

        // dbg!(&module_annotations);

        // TODO both decorators should take Rc's instead of needing to clone here
        let decorators = Decorators::new()
            .with(LocDecorator(spans.to_vec()))
            .with(TypeDecorator(module_annotations.clone()));

        // TODO I should use NodePrinter here and use the global_id.id as root
        let tree_printer = TreePrinter::new(&tree, &interner, &decorators, info.id);

        debug!(
            "{} SourceId {}, ModuleSym {}\n{}",
            "Typed Abstract Syntax Tree".bold().bright_white(),
            info.source,
            module_sym,
            tree_printer.render(print_options, arena)
        );

        type_annotations.insert(module_sym, module_annotations);
    }

    TypeCheckOutput {
        type_env,
        type_annotations,
    }
}
