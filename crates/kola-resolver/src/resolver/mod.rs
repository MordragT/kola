use std::io;

use camino::Utf8Path;
use indexmap::IndexMap;

use kola_print::prelude::*;
use kola_span::{Issue, Report, SourceManager};
use kola_tree::print::{Decorators, TreePrinter};
use kola_utils::{interner::StrInterner, io::FileSystem};
use log::debug;

use crate::{
    forest::Forest,
    info::ModuleGraph,
    prelude::Topography,
    print::ResolutionDecorator,
    resolver::{effect::EffectResolution, ty::TypeResolution},
    scope::ModuleScopes,
    symbol::{EffectSym, ModuleSym, ModuleTypeSym, TypeSym, ValueSym},
};

mod discover;
mod effect;
mod module;
mod module_ty;
mod ty;
mod value;

use discover::DiscoverOutput;
use module::ModuleResolution;
use module_ty::ModuleTypeResolution;
use value::ValueResolution;

pub type ModuleTypeOrders = IndexMap<ModuleSym, Vec<ModuleTypeSym>>;
pub type EffectOrders = IndexMap<ModuleSym, Vec<EffectSym>>;
pub type TypeOrders = IndexMap<ModuleSym, Vec<TypeSym>>;
pub type ValueOrders = IndexMap<ModuleSym, Vec<ValueSym>>;

#[derive(Debug, Clone, Default)]
pub struct ResolveOutput {
    pub source_manager: SourceManager,
    pub forest: Forest,
    pub topography: Topography,
    pub module_graph: ModuleGraph,
    pub module_scopes: ModuleScopes,
    pub module_order: Vec<ModuleSym>,
    pub module_type_orders: ModuleTypeOrders,
    pub effect_orders: EffectOrders,
    pub type_orders: TypeOrders,
    pub value_orders: ValueOrders,
    pub entry_points: Vec<ValueSym>,
}

pub fn resolve(
    path: impl AsRef<Utf8Path>,
    io: &dyn FileSystem,
    arena: &Bump,
    interner: &mut StrInterner,
    report: &mut Report,
    print_options: PrintOptions,
) -> io::Result<ResolveOutput> {
    let DiscoverOutput {
        source_manager,
        forest,
        topography,
        mut module_graph,
        module_scopes,
        functors,
        entry_points,
    } = discover::discover(path, io, arena, interner, report, print_options)?;

    if !report.is_empty() {
        return Ok(ResolveOutput {
            source_manager,
            forest,
            topography,
            module_graph,
            entry_points,
            ..Default::default()
        });
    }

    let ModuleResolution { mut module_scopes } = module::resolve_modules(
        module_scopes,
        &functors,
        interner,
        report,
        &mut module_graph,
    );

    if !report.is_empty() {
        return Ok(ResolveOutput {
            source_manager,
            forest,
            topography,
            module_graph,
            module_scopes,
            entry_points,
            ..Default::default()
        });
    }

    let ModuleTypeResolution { module_type_orders } =
        module_ty::resolve_module_types(&mut module_scopes, report, interner);

    let TypeResolution { type_orders } = ty::resolve_types(&mut module_scopes, report);

    let EffectResolution { effect_orders } = effect::resolve_effects(&mut module_scopes, report);

    let ValueResolution { value_orders } = value::resolve_values(&mut module_scopes, report);

    for (sym, scope) in &module_scopes {
        let source = scope.info.source;

        let tree = &*forest[source];
        // let spans = &*topography[source];

        // let loc_decorator = LocDecorator(spans);
        let resolution_decorator = ResolutionDecorator(&scope.resolved);
        let decorators = Decorators::new()
            // .with(&loc_decorator)
            .with(&resolution_decorator);

        let tree_printer = TreePrinter::new(tree, interner, decorators, scope.info.id);

        debug!(
            "{} SourceId {}, ModuleSym {}\n{}",
            "Resolved Abstract Syntax Tree".bold().bright_white(),
            source,
            sym,
            tree_printer.render(print_options, arena)
        );
    }

    debug!(
        "{} Module Graph:\n{}",
        "Module Graph".bold().bright_white(),
        module_graph.to_dot()
    );

    let module_order = match module_graph.topological_sort() {
        Ok(order) => order,
        Err(cycle) => {
            report.add_issue(
                Issue::error(cycle.to_string(), 0)
                    .with_help("Check for circular dependencies in module definitions."),
            );
            return Ok(ResolveOutput {
                source_manager,
                forest,
                topography,
                module_graph,
                module_scopes,
                module_type_orders,
                effect_orders,
                type_orders,
                value_orders,
                entry_points,
                ..Default::default()
            });
        }
    };

    Ok(ResolveOutput {
        source_manager,
        forest,
        topography,
        module_graph,
        module_scopes,
        module_order,
        module_type_orders,
        effect_orders,
        type_orders,
        value_orders,
        entry_points,
    })
}
