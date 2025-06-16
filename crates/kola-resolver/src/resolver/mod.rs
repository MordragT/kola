use std::io;

use camino::Utf8Path;
use indexmap::IndexMap;

use kola_print::prelude::*;
use kola_span::{Report, SourceManager};
use kola_syntax::loc::LocDecorator;
use kola_tree::print::{Decorators, TreePrinter};
use kola_utils::{interner::StrInterner, io::FileSystem};
use log::debug;

use crate::{
    forest::Forest,
    info::ModuleGraph,
    prelude::Topography,
    print::ResolutionDecorator,
    resolver::ty::TypeResolution,
    scope::ModuleScopes,
    symbol::{ModuleSym, TypeSym, ValueSym},
};

mod discover;
mod module;
mod ty;
mod value;

use discover::DiscoverOutput;
use module::ModuleResolution;
use value::ValueResolution;

pub type TypeOrders = IndexMap<ModuleSym, Vec<TypeSym>>;
pub type ValueOrders = IndexMap<ModuleSym, Vec<ValueSym>>;

#[derive(Debug, Clone, Default)]
pub struct ResolveOutput {
    pub source_manager: SourceManager,
    pub forest: Forest,
    pub topography: Topography,
    pub module_graph: ModuleGraph,
    pub module_scopes: ModuleScopes,
    pub value_orders: ValueOrders,
    pub type_orders: TypeOrders,
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

    let module_symbols = module_scopes
        .iter()
        .map(|scope| scope.info.sym)
        .collect::<Vec<_>>();

    let ModuleResolution { mut module_scopes } =
        module::resolve_modules(module_scopes, interner, report, &mut module_graph);

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

    let TypeResolution { type_orders } =
        ty::resolve_types(&module_symbols, &mut module_scopes, report);

    let ValueResolution { value_orders } =
        value::resolve_values(&module_symbols, &mut module_scopes, report);

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

    Ok(ResolveOutput {
        source_manager,
        forest,
        topography,
        module_graph,
        module_scopes,
        value_orders,
        type_orders,
        entry_points,
    })
}
