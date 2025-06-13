use kola_print::prelude::*;
use kola_resolver::{
    GlobalId, forest::Forest, info::ModuleGraph, prelude::Topography, resolver::ValueOrders,
    scope::ModuleScopes,
};
use kola_span::{IntoDiagnostic, Issue, Report};
use kola_syntax::loc::LocDecorator;
use kola_tree::{
    meta::MetaView,
    node,
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

pub struct TypeCheckInput {
    pub forest: Forest,
    pub topography: Topography,
    pub module_graph: ModuleGraph,
    pub module_scopes: ModuleScopes,
    pub value_orders: ValueOrders,
}

#[derive(Debug, Clone, Default)]
pub struct TypeCheckOutput {
    pub type_env: TypeEnv,
    pub type_annotations: TypeAnnotations,
}

pub fn type_check(
    input: TypeCheckInput,
    arena: &Bump,
    interner: &mut StrInterner,
    report: &mut Report,
    print_options: PrintOptions,
) -> TypeCheckOutput {
    let TypeCheckInput {
        forest,
        topography,
        module_graph,
        module_scopes,
        value_orders,
    } = input;

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

    dbg!(&module_order);

    for module_sym in module_order {
        let module_scope = module_scopes[&module_sym].clone();
        let info = module_scope.info;

        let tree = &*forest[info.source];
        let spans = topography[info.source].clone();
        let value_order = &value_orders[&module_sym];

        dbg!(module_sym, &value_order);

        let mut module_annotations = TypedNodes::new();

        for &value_sym in value_order {
            let id: Id<node::ValueBind> = todo!();

            let typer = Typer::new(id, spans.clone(), &type_env, &module_scope.resolved);
            let typed_nodes = match typer.solve(tree, report) {
                Ok(typed_nodes) => typed_nodes,
                Err((errors, span)) => {
                    let diag = interner.display(&errors).into_diagnostic(span);
                    report.add_diagnostic(diag);
                    continue;
                }
            };

            type_env.insert_value(value_sym, typed_nodes.meta(id).clone());
            // TODO insert more stuff into the type_env about type_binds, etc.
            module_annotations.extend(typed_nodes);
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
