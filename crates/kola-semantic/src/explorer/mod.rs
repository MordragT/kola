use kola_print::PrintOptions;
use kola_syntax::prelude::*;
use kola_vfs::prelude::*;
use kola_tree::{node::Vis, prelude::*};

use log::debug;
use miette::{Diagnostic, Result};
use owo_colors::OwoColorize;
use std::{
    collections::HashMap,
    io,
    ops::ControlFlow,
    path::{Path, PathBuf},
};
use thiserror::Error;

use crate::{
    VisitState,
    module::{
        ModuleBind, ModuleId, ModuleInfoBuilder, ModuleInfoTable, ModuleInfoView, TypeBind,
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




// pub fn explore_import(
//     path: impl AsRef<Path>,
//     id: Id<node::ModuleImport>,
//     span: Span,
//     source: &Source,
//     tree: &impl TreeView,
//     options: PrintOptions,
// ) -> Result<FileInfo, SourceReport> {
//     let path = import_path_of(path, id, tree);

//     let source = Source::from_path(&path)
//         .map_err(|e| SourceDiagnostic::error(span, e.to_string()).report(source.to_owned()))?;
//     let file = FileParser::new(source, options).try_parse()?;
//     Ok(file)
// }

// TODO better error messages

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
) -> Option<ModuleId> {
    let (id, path) = path_id.get(tree).0.split_first()?;
    let name = id.get(tree);
    let mut module_id = &module.module(name)?.id;

    for id in path {
        let name = id.get(tree);
        let module = module_infos.get(module_id)?.module(name)?;

        if module.vis != Vis::Export {
            return None;
        }
        module_id = &module.id;
    }

    Some(module_id.to_owned())
}

pub fn try_explore_path(
    path_id: Id<node::ModulePath>,
    span: Span,
    tree: &impl TreeView,
    module: &impl ModuleInfoView,
    module_infos: &ModuleInfoTable,
) -> Result<ModuleId, SourceDiagnostic> {
    explore_path(path_id, tree, module, module_infos).ok_or(
        SourceDiagnostic::error(span, "Module not found")
            .with_help("Maybe the module is not marked to be exported."),
    )
}

fn new_item(
    path: PathBuf,
    parent: Option<ModuleId>,
    options: PrintOptions,
) -> ExploreResult<(ModuleId, FileInfo, ModuleInfoBuilder)> {
    let source = Source::from_path(&path)?;
    let file = FileParser::new(source, options).try_parse()?;

    let id = file.tree.root_id();
    let span = file.span(id);

    let mut builder = ModuleInfoBuilder::new();
    if let Some(parent_id) = &parent {
        let bind = ModuleBind::new(parent_id.to_owned(), node::Vis::None, span);
        builder
            .insert_module("super".into(), bind)
            .expect("Whoops somehow the super module failed to be inserted");
    }

    let module_id = ModuleId::new(parent, path, id);

    Ok((module_id, file, builder))
}

pub fn explore(file_path: FilePath, import_path: ModulePath, options: PrintOptions) -> ExploreResult<()> {
    let mut order = Vec::new();
    let mut stack = Vec::new();
    let mut visited = HashMap::new();
    let mut module_builders = HashMap::new();
    let mut file_infos = FileInfoTable::new();
    let mut module_infos = ModuleInfoTable::new();

    let (module_id, file, builder) = new_item(file_path.clone(), None, options)?;
    module_builders.insert(module_id.clone(), builder);
    file_infos.insert(file_path.clone(), file);
    stack.push(module_id);

    while let Some(module_id) = stack.pop() {
        match visited
            .get(&module_id)
            .copied()
            .unwrap_or(VisitState::Unvisited)
        {
            // TODO maybe more states, because when in state Unvisited it is already in the items HashMap
            VisitState::Unvisited => {
                visited.insert(module_id.clone(), VisitState::Visiting);
            }
            VisitState::Visiting => {
                visited.insert(module_id.clone(), VisitState::Visited);
                order.push(module_id.clone());
                continue;
            }
            VisitState::Visited => {
                continue;
            }
        }

        let file = &file_infos[module_id.path()];
        let builder = module_builders.get_mut(&module_id).unwrap();

        let mut explorer = Explorer {
            stack: &mut stack,
            builder,
            visited: &visited,
            module_infos: &module_infos,
            file,
            module_id: &module_id,
            options,
        };

        match module_id.id().visit_by(&mut explorer, &file.tree) {
            ControlFlow::Continue(()) => (),
            ControlFlow::Break(e) => return Err(e),
        }

        let module_info = module_builders.remove(&module_id).unwrap().finish();
        module_infos.insert(module_id.clone(), module_info);
        stack.push(module_id);
    }

    // order.reverse();

    Ok(())
}

struct Explorer<'a> {
    stack: &'a mut Vec<ModuleId>,
    builder: &'a mut ModuleInfoBuilder,
    visited: &'a HashMap<ModuleId, VisitState>,
    module_infos: &'a ModuleInfoTable,
    file: &'a FileInfo,
    module_id: &'a ModuleId,
    options: PrintOptions,
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
        let parent = Some(self.module_id.to_owned());

        let submodule = match *value.get(tree) {
            node::ModuleExpr::Module(id) => {
                // TODO Do I need to do something extra here for multi nested modules ?
                // It should just be pushed to the stack and because I am not walking over it
                // the explorer should not investigate further.
                // So probably no ?
                let submodule_id = ModuleId::new(parent, self.module_id.path(), id);
                ModuleBind::new(submodule_id, vis, self.span(id))
            }
            node::ModuleExpr::Import(import_id) => {
                // TODO I might want to check if I am in a nested module, because then I should disallow imports.
                //
                // Module files are only allowed to import from their designated module direcotry.
                // For example a/b.kl may only import from a/b/
                // Therefore the parsing logic inside FileExplorer::import is guaranteed to only
                // run once per file.

                let file = explore_import(path, id, span, source, tree, options)

                let file = match self.file.explore().import(import_id, self.options) {
                    Ok(file) => file,
                    Err(e) => return ControlFlow::Break(e.into()),
                };
                let path = file.source.path().to_owned();
                let id = file.tree.root_id();
                self.world.file_infos.insert(path.clone(), file);

                let submodule_id = ModuleId::new(parent, path, id);
                ModuleBind::new(submodule_id, vis, self.span(import_id))
            }
            node::ModuleExpr::Path(path_id) => {
                let span = self.span(path_id);

                let id =
                    match try_explore_path(path_id, span, tree, self.builder, &self.module_infos) {
                        Ok(id) => id,
                        Err(diag) => return ControlFlow::Break(self.report(diag)),
                    };

                ModuleBind::new(id, vis, span)
            }
        };

        match self
            .visited
            .get(&submodule.id)
            .copied()
            .unwrap_or(VisitState::Unvisited)
        {
            VisitState::Unvisited => self.stack.push(submodule.id.clone()),
            VisitState::Visiting => {
                return ControlFlow::Break(
                    self.report(
                        SourceDiagnostic::error(
                            submodule.span,
                            format!(
                                "Circular dependency detected at: {:?}",
                                self.module_id.path()
                            ),
                        )
                        .with_help("Review its imports."),
                    ),
                );
            }
            VisitState::Visited => (),
        }

        if let Err(diag) = self.builder.insert_module(name, submodule) {
            return ControlFlow::Break(self.report(diag));
        }
        ControlFlow::Continue(())
    }
}
