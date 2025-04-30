use kola_print::PrintOptions;
use kola_syntax::prelude::*;
use kola_tree::{node::Vis, prelude::*};
use kola_vfs::prelude::*;

use miette::{Diagnostic, Result};
use std::{collections::HashMap, io, ops::ControlFlow};
use thiserror::Error;

use crate::{
    VisitState,
    module::{
        ModuleBind, ModuleInfoBuilder, ModuleInfoTable, ModuleInfoView, ModulePath, TypeBind,
        ValueBind,
    },
};

pub type ExploreResult<T> = Result<T, ExploreError>;

#[derive(Debug, Error, Diagnostic)]
pub enum ExploreError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[diagnostic(transparent)]
    #[error(transparent)]
    Source(#[from] SourceReport),
}

#[derive(Debug, Error, Diagnostic)]
#[error("Errors:")]
pub struct ExploreErrors(#[related] pub Vec<ExploreError>);

pub struct ExploreReport {
    pub errors: ExploreErrors,
    pub order: Vec<ModulePath>,
    pub file_infos: FileInfoTable,
    pub module_parents: HashMap<ModulePath, ModulePath>,
    pub module_infos: ModuleInfoTable,
}

/// Resolves the module path
/// 1. Looks for module of the first segment in the current module
/// 2. For every other segment looks in the corresponding module
///     and checks if a module exists for it and if it is marked as `export`
///
/// Returns None if some segment of the path could not be resolved
pub fn explore_path(
    path_id: Id<node::ModulePath>,
    tree: &impl TreeView,
    module: &impl ModuleInfoView,
    module_infos: &ModuleInfoTable,
) -> Option<ModulePath> {
    let (id, path) = path_id.get(tree).0.split_first()?;
    let name = id.get(tree);
    let mut module_path = &module.module(name)?.path;

    for id in path {
        let name = id.get(tree);
        let module = module_infos.get(module_path)?.module(name)?;

        if module.vis != Vis::Export {
            return None;
        }
        module_path = &module.path;
    }

    Some(module_path.to_owned())
}

pub fn try_explore_path(
    path_id: Id<node::ModulePath>,
    span: Span,
    tree: &impl TreeView,
    module: &impl ModuleInfoView,
    module_infos: &ModuleInfoTable,
) -> Result<ModulePath, SourceDiagnostic> {
    explore_path(path_id, tree, module, module_infos).ok_or(
        SourceDiagnostic::error(span, "Module not found")
            .with_help("Maybe the module is not marked to be exported."),
    )
}

pub fn init_file(
    file_path: FilePath,
    import_path: Option<ImportPath>,
    options: PrintOptions,
) -> ExploreResult<(ModulePath, FileInfo)> {
    let source = Source::from_path(file_path.clone(), import_path)?;
    let file = FileParser::new(source, options).try_parse()?;

    let id = file.tree.root_id();
    let module_path = ModulePath::new(id, file_path);

    Ok((module_path, file))
}

/// Topological DFS Algorithm
pub fn explore(
    file_path: FilePath,
    import_path: Option<ImportPath>,
    options: PrintOptions,
) -> ExploreReport {
    let mut order = Vec::new();
    let mut stack = Vec::new();
    let mut visited = HashMap::new();
    let mut file_infos = FileInfoTable::new();
    let mut module_parents = HashMap::new();
    let mut module_infos = ModuleInfoTable::new();
    let mut errors = Vec::new();

    let (module_path, file) = match init_file(file_path.clone(), import_path, options) {
        Ok(tuple) => tuple,
        Err(e) => {
            return ExploreReport {
                errors: ExploreErrors(vec![e]),
                order,
                module_parents,
                file_infos,
                module_infos,
            };
        }
    };

    file_infos.insert(file_path.clone(), file);
    stack.push(module_path);

    while let Some(module_path) = stack.pop() {
        match visited
            .get(&module_path)
            .copied()
            .unwrap_or(VisitState::Unvisited)
        {
            VisitState::Unvisited => {
                visited.insert(module_path.clone(), VisitState::Visiting);
                stack.push(module_path.clone());
            }
            VisitState::Visiting => {
                visited.insert(module_path.clone(), VisitState::Visited);
                order.push(module_path.clone());
                continue;
            }
            VisitState::Visited => {
                continue;
            }
        }

        let file = file_infos[&module_path.file_path].clone();

        let mut explorer = Explorer::new(
            &mut stack,
            &visited,
            &mut file_infos,
            &mut module_parents,
            &module_infos,
            &file,
            &module_path,
            options,
        );

        match module_path.id.visit_by(&mut explorer, &file.tree) {
            ControlFlow::Continue(()) => (),
            ControlFlow::Break(e) => {
                errors.push(e);
                continue;
            }
        }

        let module_info = explorer.builder.finish();
        module_infos.insert(module_path.clone(), module_info);
    }

    ExploreReport {
        errors: ExploreErrors(errors),
        order,
        module_parents,
        file_infos,
        module_infos,
    }
}

struct Explorer<'a> {
    stack: &'a mut Vec<ModulePath>,
    visited: &'a HashMap<ModulePath, VisitState>,
    file_infos: &'a mut FileInfoTable,
    module_parents: &'a mut HashMap<ModulePath, ModulePath>,
    module_infos: &'a ModuleInfoTable,
    file: &'a FileInfo,
    module_path: &'a ModulePath,
    builder: ModuleInfoBuilder,
    options: PrintOptions,
}

impl<'a> Explorer<'a> {
    pub fn new(
        stack: &'a mut Vec<ModulePath>,
        visited: &'a HashMap<ModulePath, VisitState>,
        file_infos: &'a mut FileInfoTable,
        module_parents: &'a mut HashMap<ModulePath, ModulePath>,
        module_infos: &'a ModuleInfoTable,
        file: &'a FileInfo,
        module_path: &'a ModulePath,
        options: PrintOptions,
    ) -> Self {
        let mut builder = ModuleInfoBuilder::new();

        if let Some(parent_path) = module_parents.get(module_path) {
            let span = file.span(module_path.id);
            let bind = ModuleBind::new(parent_path.clone(), node::Vis::None, span);
            builder
                .insert_module("super".into(), bind)
                .expect("Whoops somehow the super module failed to be inserted");
        }

        Self {
            stack,
            visited,
            file_infos,
            module_parents,
            module_infos,
            file,
            module_path,
            builder,
            options,
        }
    }
}

impl<'a> Explorer<'a> {
    #[inline]
    fn span<T>(&self, id: Id<T>) -> Span
    where
        T: MetaCast<SyntaxPhase, Meta = Span>,
    {
        self.file.span(id)
    }

    #[inline]
    fn report(&self, diag: SourceDiagnostic) -> ExploreError {
        diag.report(self.file.source.clone()).into()
    }
}

impl<'a, T: TreeView> Visitor<T> for Explorer<'a> {
    type BreakValue = ExploreError;

    // This will only traverse the current module's scope,
    // because the different module branches are not visited any further under `visit_module_bind`
    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let node::ValueBind { vis, name, .. } = *id.get(tree);

        let vis = *vis.get(tree);
        let name = name.get(tree).0.clone();

        if let Err(diag) = self
            .builder
            .insert_value(name, ValueBind::new(id, vis, span))
        {
            return ControlFlow::Break(self.report(diag));
        }

        ControlFlow::Continue(())
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let node::TypeBind { name, .. } = *id.get(tree);

        let name = name.get(tree).0.clone();

        if let Err(diag) = self.builder.insert_type(name, TypeBind::new(id, span)) {
            return ControlFlow::Break(self.report(diag));
        }
        ControlFlow::Continue(())
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleBind {
            vis,
            name,
            ty: _,
            value,
        } = *id.get(tree);

        let vis = *vis.get(tree);
        let name = name.get(tree).0.clone();
        let parent_path = self.module_path.to_owned();

        let bind = match *value.get(tree) {
            node::ModuleExpr::Module(id) => {
                let span = self.span(id);
                let module_path = ModulePath::new(id, self.module_path.file_path.clone());

                self.module_parents.insert(module_path.clone(), parent_path);
                ModuleBind::new(module_path, vis, span)
            }
            node::ModuleExpr::Import(import_id) => {
                let name = import_id.get(tree).0.get(tree);

                // TODO when inside a nested module this will still return Ok
                // because it will just use the root file
                // It should however return an error
                let import_path = match self.file.try_import_path() {
                    Ok(path) => path,
                    Err(e) => return ControlFlow::Break(e.into()),
                };

                let (file_path, import_path) = match import_path.discover(name) {
                    Ok(tuple) => tuple,
                    Err(e) => return ControlFlow::Break(e.into()),
                };

                // Module files are only allowed to import from their designated module direcotry.
                // For example a/b.kl may only import from a/b/
                // Therefore the parsing logic inside `explore_file` is guaranteed to only
                // run once per file.
                let (module_path, file) =
                    match init_file(file_path.clone(), import_path, self.options) {
                        Ok(tuple) => tuple,
                        Err(e) => return ControlFlow::Break(e),
                    };
                self.module_parents.insert(module_path.clone(), parent_path);
                self.file_infos.insert(file_path, file);

                ModuleBind::new(module_path, vis, self.span(import_id))
            }
            node::ModuleExpr::Path(path_id) => {
                let span = self.span(path_id);

                let path = match try_explore_path(
                    path_id,
                    span,
                    tree,
                    &self.builder,
                    &self.module_infos,
                ) {
                    Ok(path) => path,
                    Err(diag) => return ControlFlow::Break(self.report(diag)),
                };

                ModuleBind::new(path, vis, span)
            }
        };

        match self
            .visited
            .get(&bind.path)
            .copied()
            .unwrap_or(VisitState::Unvisited)
        {
            VisitState::Unvisited => self.stack.push(bind.path.clone()),
            VisitState::Visiting => {
                return ControlFlow::Break(
                    self.report(
                        SourceDiagnostic::error(
                            bind.span,
                            format!("Circular dependency detected at: {:?}", self.module_path),
                        )
                        .with_help("Review its imports."),
                    ),
                );
            }
            VisitState::Visited => (),
        }

        if let Err(diag) = self.builder.insert_module(name, bind) {
            return ControlFlow::Break(self.report(diag));
        }
        ControlFlow::Continue(())
    }
}
