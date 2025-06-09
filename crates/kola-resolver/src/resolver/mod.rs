use std::{io, rc::Rc};

use camino::Utf8Path;
use indexmap::IndexMap;

use kola_print::prelude::*;
use kola_span::{Report, SourceManager};
use kola_utils::{dependency::DependencyGraph, interner::StrInterner, io::FileSystem};

use crate::{
    forest::Forest,
    prelude::Topography,
    resolver::{discover::DiscoverOutput, module::ModuleResolution, value::resolve_values},
    scope::ModuleScope,
    symbol::{ModuleSym, SymbolTable},
};

mod discover;
mod module;
mod value;

pub type ModuleGraph = DependencyGraph<ModuleSym>;
pub type MutModuleScopes = IndexMap<ModuleSym, ModuleScope>;
pub type ModuleScopes = IndexMap<ModuleSym, Rc<ModuleScope>>;

#[derive(Debug, Clone, Default)]
pub struct ResolveOutput {
    pub source_manager: SourceManager,
    pub forest: Forest,
    pub topography: Topography,
    pub symbol_table: SymbolTable,
    pub module_graph: ModuleGraph,
    pub module_scopes: ModuleScopes,
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
        mut symbol_table,
        mut module_graph,
        module_scopes,
    } = discover::discover(path, io, arena, interner, report, print_options)?;

    if !report.is_empty() {
        return Ok(ResolveOutput {
            source_manager,
            forest,
            topography,
            symbol_table,
            module_graph,
            ..Default::default()
        });
    }

    // I could replace this with a topological sort, if I need it later anyway.
    // For that to work, I should however ensure that the module graph has no duplicates,
    // or at least that the duplicates are handled correctly.
    let module_symbols = module_scopes.keys().copied().collect::<Vec<_>>();

    let ModuleResolution { module_scopes } =
        module::resolve_modules(module_scopes, interner, report, &mut module_graph);

    if !report.is_empty() {
        return Ok(ResolveOutput {
            source_manager,
            forest,
            topography,
            symbol_table,
            module_graph,
            module_scopes,
        });
    }

    resolve_values(
        &module_symbols,
        &forest,
        &topography,
        &module_scopes,
        report,
        &mut symbol_table,
    );

    Ok(ResolveOutput {
        source_manager,
        forest,
        topography,
        symbol_table,
        module_graph,
        module_scopes,
    })
}
