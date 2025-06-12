use std::io;

use camino::Utf8Path;
use indexmap::IndexMap;

use kola_print::prelude::*;
use kola_span::{Report, SourceManager};
use kola_utils::{interner::StrInterner, io::FileSystem};

use crate::{
    bind::Bindings,
    forest::Forest,
    info::ModuleGraph,
    prelude::Topography,
    scope::ModuleCells,
    symbol::{ModuleSym, ValueSym},
};

mod discover;
mod module;
mod value;

use discover::DiscoverOutput;
use module::ModuleResolution;
use value::ValueResolution;

pub type ValueOrders = IndexMap<ModuleSym, Vec<ValueSym>>;

#[derive(Debug, Clone, Default)]
pub struct ResolveOutput {
    pub source_manager: SourceManager,
    pub forest: Forest,
    pub topography: Topography,
    pub bindings: Bindings,
    pub module_graph: ModuleGraph,
    pub module_scopes: ModuleCells,
    pub value_orders: ValueOrders,
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
        mut bindings,
        mut module_graph,
        module_scopes,
    } = discover::discover(path, io, arena, interner, report, print_options)?;

    if !report.is_empty() {
        return Ok(ResolveOutput {
            source_manager,
            forest,
            topography,
            bindings,
            module_graph,
            ..Default::default()
        });
    }

    let module_symbols = module_scopes
        .iter()
        .map(|scope| scope.bind.sym())
        .collect::<Vec<_>>();

    let ModuleResolution { module_scopes } =
        module::resolve_modules(module_scopes, interner, report, &mut module_graph);

    if !report.is_empty() {
        return Ok(ResolveOutput {
            source_manager,
            forest,
            topography,
            bindings,
            module_graph,
            module_scopes,
            ..Default::default()
        });
    }

    let ValueResolution { value_orders } =
        value::resolve_values(&module_symbols, &module_scopes, report, &mut bindings);

    Ok(ResolveOutput {
        source_manager,
        forest,
        topography,
        bindings,
        module_graph,
        module_scopes,
        value_orders,
    })
}
