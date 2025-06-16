use kola_print::prelude::*;
use kola_resolver::{
    forest::Forest,
    info::ModuleGraph,
    prelude::Topography,
    print::ResolutionDecorator,
    resolver::{TypeOrders, ValueOrders},
    scope::ModuleScopes,
};
use kola_span::{Issue, Report};
use kola_syntax::loc::LocDecorator;
use kola_tree::{
    meta::MetaView,
    print::{Decorators, TreePrinter},
};
use kola_utils::interner::StrInterner;
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

            let Some(typed_nodes) = typer.solve(tree, interner, report) else {
                // If there are errors in type checking types, break out early
                break;
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

            let Some(typed_nodes) = typer.solve(tree, interner, report) else {
                // If there are errors in type checking types, break out early
                break;
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

        let resolution_decorator = ResolutionDecorator(&module_scope.resolved);
        let type_decorator = TypeDecorator(&module_annotations);
        let decorators = Decorators::new()
            .with(&resolution_decorator)
            .with(&type_decorator);

        let tree_printer = TreePrinter::new(&tree, &interner, decorators, info.id);

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
