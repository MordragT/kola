use std::io;

use camino::Utf8Path;
use indexmap::IndexMap;

use kola_print::prelude::*;
use kola_span::{Report, SourceManager};
use kola_utils::{interner::StrInterner, io::FileSystem};

use crate::{
    forest::Forest,
    info::ModuleGraph,
    prelude::Topography,
    scope::ModuleScopes,
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
    pub module_graph: ModuleGraph,
    pub module_scopes: ModuleScopes,
    pub value_orders: ValueOrders,
    pub entry_point: ValueSym,
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

    // TODO better error handling with trace
    let &[entry_point] = entry_points.as_slice() else {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "No entry point or too many found in the project",
        ));
    };

    if !report.is_empty() {
        return Ok(ResolveOutput {
            source_manager,
            forest,
            topography,
            module_graph,
            entry_point,
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
            entry_point,
            ..Default::default()
        });
    }

    let ValueResolution { value_orders } =
        value::resolve_values(&module_symbols, &mut module_scopes, report);

    Ok(ResolveOutput {
        source_manager,
        forest,
        topography,
        module_graph,
        module_scopes,
        value_orders,
        entry_point,
    })
}
