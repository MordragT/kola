use kola_print::PrintOptions;
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use log::debug;
use miette::{Diagnostic, Result};
use owo_colors::OwoColorize;
use std::{collections::HashMap, io, ops::ControlFlow, path::Path};
use thiserror::Error;

use crate::{
    file::{FileInfo, FileInfoTable, FileParser},
    module::{
        ModuleBind, ModuleExplorer, ModuleId, ModuleInfo, ModuleInfoBuilder, ModuleInfoTable,
        ValueBind,
    },
    typer::{self, TypeDecorator, TypeInfo, TypeInfoTable, Typer},
};

pub struct World {
    pub order: Vec<ModuleId>,
    pub file_infos: FileInfoTable,
    pub module_infos: ModuleInfoTable,
    pub type_infos: TypeInfoTable,
    pub options: PrintOptions,
}

impl World {
    pub fn new(options: PrintOptions) -> Self {
        Self {
            order: Vec::new(),
            file_infos: HashMap::new(),
            module_infos: HashMap::new(),
            type_infos: HashMap::new(),
            options,
        }
    }

    pub fn file_info(&self, id: &ModuleId) -> FileInfo {
        self.file_infos[id.path()].clone()
    }

    pub fn module_info(&self, id: &ModuleId) -> ModuleInfo {
        self.module_infos[id].clone()
    }

    pub fn type_info(&self, id: &ModuleId) -> TypeInfo {
        self.type_infos[id].clone()
    }
}

pub type ExploreResult<T> = Result<T, ExploreError>;

#[derive(Debug, Error, Diagnostic)]
pub enum ExploreError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[diagnostic(transparent)]
    #[error(transparent)]
    Source(#[from] SourceReport),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum VisitState {
    Unvisited,
    Visiting,
    Visited,
}

impl World {
    // TODO what happens with nested modules ?
    // TODO external packages could be provided also via HashMap<ModuleId, ModuleInfo>

    /// Topological DFS Algorithm
    pub fn explore(&mut self, path: impl AsRef<Path>) -> ExploreResult<&mut Self> {
        let path = path.as_ref().canonicalize()?;
        let source = Source::from_path(&path)?;
        let file = FileParser::new(source, self.options).try_parse()?;
        let id = file.tree.root_id();
        let module_id = ModuleId::root(path.clone(), id);
        self.file_infos.insert(path.clone(), file);

        let mut stack = vec![module_id];
        let mut visited = HashMap::new();

        while let Some(module_id) = stack.last().cloned() {
            match visited
                .get(&module_id)
                .copied()
                .unwrap_or(VisitState::Unvisited)
            {
                VisitState::Unvisited => {
                    visited.insert(module_id.clone(), VisitState::Visiting);
                }
                VisitState::Visiting => {
                    visited.insert(module_id.clone(), VisitState::Visited);
                    self.order.push(module_id.clone());
                    stack.pop();
                    continue;
                }
                VisitState::Visited => {
                    stack.pop();
                    continue;
                }
            }

            let file = self.file_info(&module_id);
            let mut explorer = Explorer::new(
                self,
                &mut stack,
                &mut visited,
                module_id.clone(),
                file.clone(),
                self.options,
            );

            match module_id.id().visit_by(&mut explorer, &file.tree) {
                ControlFlow::Continue(()) => (),
                ControlFlow::Break(e) => return Err(e),
            }

            let info = explorer.finish();
            self.module_infos.insert(module_id, info);
        }

        // order.reverse();

        Ok(self)
    }
}

impl World {
    pub fn type_check(&mut self) -> Result<&mut Self, typer::Error> {
        for module_id in &self.order {
            let module = self.module_info(module_id);
            let FileInfo {
                tree,
                spans,
                source,
            } = self.file_info(module_id);

            let types = Typer::new(
                module_id.clone(),
                module,
                spans.clone(),
                &self.module_infos,
                &self.type_infos,
            )
            .solve(&tree)?;
            self.type_infos.insert(module_id.to_owned(), types.clone());

            debug!(
                "{} {:?}\n{}",
                "Typed Abstract Syntax Tree".bold().bright_white(),
                source.path(),
                TreePrinter::new(&tree)
                    .with(SpanDecorator(spans))
                    .with(TypeDecorator(types))
                    .render_at(module_id.id(), self.options)
            );
        }

        Ok(self)
    }
}

struct Explorer<'a> {
    world: &'a mut World,
    stack: &'a mut Vec<ModuleId>,
    visited: &'a mut HashMap<ModuleId, VisitState>,
    module_id: ModuleId,
    builder: ModuleInfoBuilder,
    file: FileInfo,
    options: PrintOptions,
}

impl<'a> Explorer<'a> {
    fn new(
        world: &'a mut World,
        stack: &'a mut Vec<ModuleId>,
        visited: &'a mut HashMap<ModuleId, VisitState>,
        module_id: ModuleId,
        file: FileInfo,
        options: PrintOptions,
    ) -> Self {
        let mut builder = ModuleInfoBuilder::new();

        if let Some(parent_id) = module_id.parent() {
            let bind = ModuleBind::new(parent_id.to_owned(), node::Vis::None);
            builder.insert_module("super".into(), bind);
        }

        Self {
            module_id,
            builder,
            world,
            stack,
            visited,
            file,
            options,
        }
    }

    fn span<T>(&self, id: Id<T>) -> Span
    where
        T: MetaCast<SyntaxPhase, Meta = Span>,
    {
        *self.file.spans.meta(id)
    }

    fn report(&self, diag: SourceDiagnostic) -> ExploreError {
        diag.report(self.file.source.clone()).into()
    }

    fn finish(self) -> ModuleInfo {
        self.builder.finish()
    }
}

// TODO maybe also visit value and type binds
// and create a mapping of Symbols to their node id's
// then in inference the symbols can be used to look up the node id's
// which in turn can be used to lookup the type in the type-table
impl<'a, T: TreeView> Visitor<T> for Explorer<'a> {
    type BreakValue = ExploreError;

    // This will only traverse the current module's scope,
    // because the different module branches are not visited any further under `visit_module_bind`
    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ValueBind {
            vis,
            name,
            ty: _,
            value,
        } = *id.get(tree);

        let vis = *vis.get(tree);
        let name = name.get(tree);

        self.builder
            .insert_value(name.0.clone(), ValueBind::new(value, vis));

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

        let (submodule, span) = match *value.get(tree) {
            node::ModuleExpr::Module(id) => {
                // TODO Do I need to do something extra here for multi nested modules ?
                // It should just be pushed to the stack and because I am not walking over it
                // the explorer should not investigate further.
                // So probably no ?
                let submodule_id = ModuleId::new(self.module_id.clone(), self.module_id.path(), id);
                (ModuleBind::new(submodule_id, vis), self.span(id))
            }
            node::ModuleExpr::Import(import_id) => {
                // TODO I might want to check if I am in a nested module, because then I should disallow imports.
                //
                // Module files are only allowed to import from their designated module direcotry.
                // For example a/b.kl may only import from a/b/
                // Therefore the parsing logic inside FileExplorer::import is guaranteed to only
                // run once per file.
                let file = match self.file.explore().import(import_id, self.options) {
                    Ok(file) => file,
                    Err(e) => return ControlFlow::Break(e.into()),
                };
                let path = file.source.path().to_owned();
                let id = file.tree.root_id();
                self.world.file_infos.insert(path.clone(), file);

                let submodule_id = ModuleId::new(self.module_id.clone(), path, id);
                (ModuleBind::new(submodule_id, vis), self.span(import_id))
            }
            node::ModuleExpr::Path(path_id) => {
                let id = match ModuleExplorer::new(
                    tree,
                    &self.module_id,
                    &self.builder,
                    &self.world.module_infos,
                )
                .module_path(path_id)
                {
                    Some(id) => id,
                    None => {
                        return ControlFlow::Break(
                            self.report(
                                SourceDiagnostic::error(self.span(path_id), "Module not found")
                                    .with_help("Maybe the module is not marked to be exported."),
                            ),
                        );
                    }
                };

                (ModuleBind::new(id, vis), self.span(path_id))
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
                            span,
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

        self.builder.insert_module(name, submodule);
        ControlFlow::Continue(())
    }
}
