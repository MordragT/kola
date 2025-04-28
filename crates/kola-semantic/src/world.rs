use kola_print::PrintOptions;
use kola_syntax::prelude::*;
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
    file::{FileInfo, FileParser},
    module::{ModuleBind, ModuleExplorer, ModuleId, ModuleInfo},
    typer::{self, TypeDecorator, TypeInfo, Typer},
};

// maybe this could use some DAG Graph for modelling dependencies which in turn allows for parallel module elaboration
pub struct World {
    pub order: Vec<ModuleId>,
    pub file_infos: HashMap<PathBuf, FileInfo>,
    pub module_infos: HashMap<ModuleId, ModuleInfo>,
    pub type_infos: HashMap<ModuleId, TypeInfo>,
}

impl World {
    pub fn new() -> Self {
        Self {
            order: Vec::new(),
            file_infos: HashMap::new(),
            module_infos: HashMap::new(),
            type_infos: HashMap::new(),
        }
    }

    pub fn file_info(&self, id: &ModuleId) -> FileInfo {
        self.file_infos[id.path()].clone()
    }

    pub fn module_info(&self, id: &ModuleId) -> &ModuleInfo {
        &self.module_infos[id]
    }

    pub fn type_info(&self, id: &ModuleId) -> TypeInfo {
        self.type_infos[id].clone()
    }
}

pub type ExploreResult<T> = Result<T, ExploreError>;

#[derive(Debug, Error, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[error("Circular dependency detected at: {0}")]
pub struct Cycle(PathBuf);

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
    // TODO module shoud only allow exports which are annotated with the "export" keyword (and prober error message should be returned)
    // TODO external packages could be provided also via HashMap<ModuleId, ModuleInfo>

    /// Topological DFS Algorithm
    pub fn explore(&mut self, path: impl AsRef<Path>) -> ExploreResult<&mut Self> {
        let path = path.as_ref().canonicalize()?;
        let source = Source::from_path(&path)?;
        let file = FileParser::new(source).try_parse()?;
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
            );

            match module_id.id().visit_by(&mut explorer, &file.tree) {
                ControlFlow::Continue(()) => (),
                ControlFlow::Break(e) => return Err(e),
            }

            let info = explorer.module;
            self.module_infos.insert(module_id, info);
        }

        // order.reverse();

        Ok(self)
    }
}

impl World {
    pub fn infer(&mut self, options: PrintOptions) -> Result<&mut Self, typer::Error> {
        for module_id in &self.order {
            let FileInfo {
                source: _,
                tree,
                spans,
            } = self.file_info(module_id);

            let types = Typer::new(&tree, spans.clone()).solve(tree.root_id(), &tree)?;
            self.type_infos.insert(module_id.to_owned(), types.clone());

            debug!("\n{}", "Typed Abstract Syntax Tree".bold().bright_white());
            debug!(
                "{}",
                TreePrinter::new(&tree)
                    .with(SpanDecorator(spans))
                    .with(TypeDecorator(types))
                    .render_at(module_id.id(), options)
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
    module: ModuleInfo,
    file: FileInfo,
}

impl<'a> Explorer<'a> {
    fn new(
        world: &'a mut World,
        stack: &'a mut Vec<ModuleId>,
        visited: &'a mut HashMap<ModuleId, VisitState>,
        module_id: ModuleId,
        file: FileInfo,
    ) -> Self {
        let mut info = ModuleInfo::new();

        if let Some(parent_id) = module_id.parent() {
            let bind = ModuleBind::new(parent_id.to_owned(), Vis::None);
            info.insert("super".into(), bind);
        }

        Self {
            module_id,
            module: info,
            world,
            stack,
            visited,
            file,
        }
    }

    pub fn span<T>(&self, id: Id<T>) -> Span
    where
        T: MetaCast<SyntaxPhase, Meta = Span>,
    {
        self.file.spans.get(id).inner_copied()
    }
}

impl<'a, T: TreeAccess> Visitor<T> for Explorer<'a> {
    type BreakValue = ExploreError;

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
                let submodule_id = ModuleId::new(self.module_id.clone(), self.module_id.path(), id);
                (ModuleBind::new(submodule_id, vis), self.span(id))
            }
            node::ModuleExpr::Import(import_id) => {
                // Because imports are file based and strictly hierarchically (a module can always only have one parent)
                // We can just parse here and be confident that we didn't parse before.
                let file = match self.file.explore().import(import_id) {
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
                let id = match ModuleExplorer::new(tree, &self.module, &self.world.module_infos)
                    .explore_path(path_id)
                {
                    Some(id) => id,
                    None => {
                        return ControlFlow::Break(ExploreError::Source(
                            SourceDiagnostic::error(
                                self.span(path_id),
                                "Module not found".to_owned(),
                            )
                            .report(self.file.source.clone()),
                        ));
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
                return ControlFlow::Break(ExploreError::Source(
                    SourceDiagnostic::error(
                        span,
                        Cycle(self.module_id.path().to_owned()).to_string(),
                    )
                    .report(self.file.source.clone()),
                ));
            }
            VisitState::Visited => (),
        }

        self.module.insert(name, submodule);
        ControlFlow::Continue(())
    }
}
