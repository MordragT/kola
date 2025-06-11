use camino::Utf8Path;
use indexmap::IndexMap;
use log::debug;
use owo_colors::OwoColorize;
use std::{io, ops::ControlFlow};

use kola_print::prelude::*;
use kola_span::{IntoDiagnostic, Loc, Report, SourceManager};
use kola_syntax::prelude::*;
use kola_tree::{node::Vis, prelude::*};
use kola_utils::{
    interner::{PathKey, StrInterner},
    io::FileSystem,
    tracker::Tracker,
};

use crate::{
    QualId,
    forest::Forest,
    info::BindInfo,
    prelude::Topography,
    resolver::MutModuleScopes,
    scope::ModuleScope,
    symbol::{ModuleSym, SymbolTable, TypeSym, ValueSym},
};

use super::ModuleGraph;

#[derive(Debug, Clone, Default)]
pub struct DiscoverOutput {
    pub source_manager: SourceManager,
    pub forest: Forest,
    pub topography: Topography,
    pub symbol_table: SymbolTable,
    pub module_graph: ModuleGraph,
    pub module_scopes: MutModuleScopes,
}

pub fn discover(
    path: impl AsRef<Utf8Path>,
    io: &dyn FileSystem,
    arena: &Bump,
    interner: &mut StrInterner,
    report: &mut Report,
    print_options: PrintOptions,
) -> io::Result<DiscoverOutput> {
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
        return Ok(DiscoverOutput {
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
        return Ok(DiscoverOutput {
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

    let mut symbol_table = SymbolTable::new();
    let mut module_graph = ModuleGraph::new();
    let mut module_scopes = IndexMap::new();

    _discover(
        path_key,
        ModuleSym::new(),
        io,
        arena,
        interner,
        report,
        &mut source_manager,
        &mut forest,
        &mut topography,
        &mut symbol_table,
        &mut module_graph,
        &mut module_scopes,
        print_options,
    );

    Ok(DiscoverOutput {
        source_manager,
        forest,
        topography,
        symbol_table,
        module_graph,
        module_scopes,
    })
}

fn _discover(
    path_key: PathKey,
    module_sym: ModuleSym,
    io: &dyn FileSystem,
    arena: &Bump,
    interner: &mut StrInterner,
    report: &mut Report,
    source_manager: &mut SourceManager,
    forest: &mut Forest,
    topography: &mut Topography,
    symbol_table: &mut SymbolTable,
    module_graph: &mut ModuleGraph,
    module_scopes: &mut MutModuleScopes,
    print_options: PrintOptions,
) {
    let tree = forest.tree(path_key);

    // Create a visitor to walk the tree and collect declarations
    let mut discoverer = Discoverer::new(
        path_key,
        module_sym,
        io,
        arena,
        interner,
        report,
        source_manager,
        forest,
        topography,
        symbol_table,
        module_graph,
        module_scopes,
        print_options,
    );

    match tree.root_id().visit_by(&mut discoverer, &*tree) {
        ControlFlow::Continue(()) => (),
        ControlFlow::Break(_) => unreachable!(),
    }

    let tracker = discoverer.tracker;

    for scope in tracker.into_completed() {
        module_scopes.insert(scope.symbol(), scope);
    }
}

struct Discoverer<'a> {
    path_key: PathKey,
    current_sym: Option<ModuleSym>,
    tracker: Tracker<ModuleScope>,
    io: &'a dyn FileSystem,
    arena: &'a Bump,
    interner: &'a mut StrInterner,
    report: &'a mut Report,
    source_manager: &'a mut SourceManager,
    forest: &'a mut Forest,
    topography: &'a mut Topography,
    symbol_table: &'a mut SymbolTable,
    module_graph: &'a mut ModuleGraph,
    module_scopes: &'a mut MutModuleScopes,
    print_options: PrintOptions,
}

impl<'a> Discoverer<'a> {
    fn new(
        path_key: PathKey,
        module_sym: ModuleSym,
        io: &'a dyn FileSystem,
        arena: &'a Bump,
        interner: &'a mut StrInterner,
        report: &'a mut Report,
        source_manager: &'a mut SourceManager,
        forest: &'a mut Forest,
        topography: &'a mut Topography,
        symbol_table: &'a mut SymbolTable,
        module_graph: &'a mut ModuleGraph,
        module_scopes: &'a mut MutModuleScopes,
        print_options: PrintOptions,
    ) -> Self {
        Self {
            path_key,
            current_sym: Some(module_sym),
            tracker: Tracker::new(),
            io,
            arena,
            interner,
            report,
            source_manager,
            forest,
            topography,
            symbol_table,
            module_graph,
            module_scopes,
            print_options,
        }
    }

    #[inline]
    pub fn span<T>(&self, id: Id<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        self.topography.span((self.path_key, id))
    }

    #[inline]
    pub fn qual<T>(&self, id: Id<T>) -> QualId<T> {
        (self.path_key, id)
    }

    #[inline]
    pub fn current_module(&self) -> &ModuleScope {
        self.tracker.current().unwrap()
    }

    #[inline]
    pub fn current_module_mut(&mut self) -> &mut ModuleScope {
        self.tracker.current_mut().unwrap()
    }
}

impl<'a, T> Visitor<T> for Discoverer<'a>
where
    T: TreeView,
{
    type BreakValue = !;

    fn visit_module(&mut self, id: Id<node::Module>, tree: &T) -> ControlFlow<Self::BreakValue> {
        let module_sym = self.current_sym.take().unwrap();
        let qual_id = self.qual(id);

        self.symbol_table.insert_module(qual_id, module_sym);

        // Add dependency from current module to this new module
        if let Some(current_scope) = self.tracker.current() {
            self.module_graph
                .add_dependency(current_scope.symbol(), module_sym);
        }

        // Create a new scope for this module
        self.tracker
            .start(ModuleScope::new(qual_id, module_sym, self.span(id)));

        // Walk through children nodes
        self.walk_module(id, tree)?;

        // Pop the scope now that we're done with this module
        self.tracker.finish();

        ControlFlow::Continue(())
    }

    fn visit_module_import(
        &mut self,
        id: Id<node::ModuleImport>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let module_sym = self.current_sym.take().unwrap();

        self.symbol_table
            .insert_module_import(self.qual(id), module_sym);

        let current_module_sym = self.current_module().symbol();

        self.module_graph
            .add_dependency(current_module_sym, module_sym);

        let import = tree.node(id);
        let name = tree.node(import.0);

        let path = match self
            .source_manager
            .resolve_import(self.path_key, name.0, &self.interner)
        {
            Ok(path) => path,
            Err(e) => {
                self.report
                    .add_diagnostic(e.into_diagnostic(self.span(id)).with_help("resolve_import"));
                return ControlFlow::Continue(());
            }
        };

        let (path_key, source) = match self.source_manager.fetch(path.as_path(), &self.io) {
            Ok(tuple) => tuple,
            Err(e) => {
                self.report
                    .add_diagnostic(e.into_diagnostic(self.span(id)).with_help("fetch"));
                return ControlFlow::Continue(());
            }
        };

        debug!(
            "{} {}\n{}",
            "Source".bold().bright_white(),
            &path,
            source.text()
        );

        let input = LexInput::new(path_key, source.text());
        let Some(tokens) = tokenize(input, &mut self.report) else {
            return ControlFlow::Continue(());
        };

        debug!(
            "{} {:?}\n{}",
            "Tokens".bold().bright_white(),
            &path,
            TokenPrinter(&tokens, self.print_options).render(self.print_options, &self.arena)
        );

        let input = ParseInput::new(path_key, tokens);

        let ParseOutput { tree, spans, .. } = parse(input, &mut self.interner, &mut self.report);

        let Some(tree) = tree else {
            return ControlFlow::Continue(());
        };

        let decorators = Decorators::new().with(LocDecorator(spans.clone()));
        let tree_printer = TreePrinter::new(&tree, &self.interner, &decorators);

        debug!(
            "{} {:?}\n{}",
            "Untyped Abstract Syntax Tree".bold().bright_white(),
            &path,
            tree_printer.render(self.print_options, &self.arena)
        );

        self.forest.insert(path_key, tree);
        self.topography.insert(path_key, spans);

        _discover(
            path_key,
            module_sym,
            self.io,
            self.arena,
            self.interner,
            self.report,
            self.source_manager,
            self.forest,
            self.topography,
            self.symbol_table,
            self.module_graph,
            self.module_scopes,
            self.print_options,
        );

        ControlFlow::Continue(())
    }

    fn visit_module_path(
        &mut self,
        id: Id<node::ModulePath>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        // This will create a new module symbol if not visited from a module bind
        let module_sym = self.current_sym.take().unwrap_or_default();

        self.symbol_table
            .insert_module_path(self.qual(id), module_sym);

        let path = tree
            .node(id)
            .0
            .iter()
            .copied()
            .map(|id| *tree.node(id))
            .collect::<Vec<_>>();

        self.current_module_mut().insert_path(module_sym, path);

        ControlFlow::Continue(())
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleBind { vis, name, .. } = *tree.node(id);

        let name = *tree.node(name);
        let vis = tree.node(vis);

        let span = self.span(id);
        let qual_id = self.qual(id);
        let module_sym = ModuleSym::new();

        self.current_sym = Some(module_sym);
        self.symbol_table.insert_module_bind(qual_id, module_sym);

        // Register the module binding in the current scope
        if let Err(e) =
            self.current_module_mut()
                .insert_module(name, module_sym, BindInfo::new(span, *vis))
        {
            self.report.add_diagnostic(e.into());
        }

        self.walk_module_bind(id, tree)
    }

    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueBind { vis, name, .. } = *tree.node(id);

        let name = *tree.node(name);
        let vis = tree.node(vis);

        let span = self.span(id);
        let qual_id = self.qual(id);
        let value_sym = ValueSym::new();

        self.symbol_table.insert_value_bind(qual_id, value_sym);

        // Register the value binding in the current scope
        if let Err(e) =
            self.current_module_mut()
                .insert_value(name, value_sym, BindInfo::new(span, *vis))
        {
            self.report.add_diagnostic(e.into());
        }

        self.walk_value_bind(id, tree) // walk to discover module paths in expressions
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::TypeBind { name, .. } = *tree.node(id);
        let name = *tree.node(name);

        let span = self.span(id);
        let qual_id = self.qual(id);
        let type_sym = TypeSym::new();

        self.symbol_table.insert_type_bind(qual_id, type_sym);

        // Register the type binding in the current scope
        if let Err(e) =
            self.current_module_mut()
                .insert_type(name, type_sym, BindInfo::new(span, Vis::Export))
        {
            self.report.add_diagnostic(e.into());
        }

        // self.walk_type_bind(id, tree) // TODO walk to discover module paths in type expressions
        ControlFlow::Continue(()) // nothing to do here at the moment
    }
}
