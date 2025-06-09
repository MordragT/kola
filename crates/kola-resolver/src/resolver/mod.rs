use std::{io, rc::Rc};

use camino::Utf8Path;
use indexmap::IndexMap;
use log::debug;
use owo_colors::OwoColorize;

use kola_print::prelude::*;
use kola_span::{Report, SourceManager};
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_utils::{dependency::DependencyGraph, interner::StrInterner, io::FileSystem};

use crate::{
    forest::Forest,
    prelude::Topography,
    resolver::{discover::DiscoverOutput, module::ModuleResolution, value::resolve_values},
    scope::ModuleScope,
    symbol::{LookupTable, ModuleSym},
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
    pub lookup_table: LookupTable,
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
    // let mut ctx = ResolveContext::new(io, print_options);

    let mut source_manager = SourceManager::new();

    let path = io.canonicalize(path.as_ref())?;

    let (path_key, source) = source_manager.fetch(path.as_path(), &io)?;

    debug!(
        "{} {}\n{}",
        "Source".bold().bright_white(),
        &path,
        source.text()
    );

    let input = LexInput::new(path_key, source.text());
    let Some(tokens) = tokenize(input, report) else {
        return Ok(ResolveOutput {
            source_manager,
            ..Default::default()
        });
    };

    debug!(
        "{} {:?}\n{}",
        "Tokens".bold().bright_white(),
        &path,
        TokenPrinter(&tokens, print_options).render(print_options, arena)
    );

    let input = ParseInput::new(path_key, tokens);
    let ParseOutput { tree, spans, .. } = parse(input, interner, report);

    let Some(tree) = tree else {
        return Ok(ResolveOutput {
            source_manager,
            ..Default::default()
        });
    };

    let decorators = Decorators::new().with(LocDecorator(spans.clone()));
    let tree_printer = TreePrinter::new(&tree, interner, &decorators);

    debug!(
        "{} {:?}\n{}",
        "Untyped Abstract Syntax Tree".bold().bright_white(),
        &path,
        tree_printer.render(print_options, arena)
    );

    let mut forest = Forest::new();
    let mut topography = Topography::new();

    forest.insert(path_key, tree);
    topography.insert(path_key, spans);

    let module_sym = ModuleSym::new();

    let DiscoverOutput {
        mut lookup_table,
        mut module_graph,
        module_scopes,
    } = discover::discover(
        path_key,
        module_sym,
        io,
        arena,
        interner,
        report,
        &mut source_manager,
        &mut forest,
        &mut topography,
        print_options,
    );

    if !report.is_empty() {
        return Ok(ResolveOutput {
            source_manager,
            forest,
            topography,
            lookup_table,
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
            lookup_table,
            module_graph,
            module_scopes,
        });
    }

    resolve_values(
        &module_symbols,
        &forest,
        &topography,
        &module_scopes,
        interner,
        report,
        &mut lookup_table,
    );

    Ok(ResolveOutput {
        source_manager,
        forest,
        topography,
        lookup_table,
        module_graph,
        module_scopes,
    })
}
