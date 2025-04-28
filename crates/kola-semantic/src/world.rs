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
    file::{FileError, FileExplorer, FileInfo, FileParser},
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

#[derive(Debug, Error, Diagnostic)]
pub enum ExploreError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[diagnostic(transparent)]
    #[error(transparent)]
    Source(#[from] SourceReport),
    #[error("Circular dependency detected at: {0}")]
    Cycle(PathBuf),
    #[error("Module could not be found")]
    ModuleNotFound, // TODO better handling of error
}

impl From<FileError> for ExploreError {
    fn from(e: FileError) -> Self {
        match e {
            FileError::Io(e) => Self::Io(e),
            FileError::Source(e) => Self::Source(e),
        }
    }
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

            let tree = self.file_info(&module_id).tree;
            let mut explorer = Explorer::new(self, &mut stack, &mut visited, module_id.clone());

            match module_id.id().visit_by(&mut explorer, &tree) {
                ControlFlow::Continue(()) => (),
                ControlFlow::Break(cycle) => return Err(cycle.into()),
            }

            let info = explorer.info;
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
    info: ModuleInfo,
}

impl<'a> Explorer<'a> {
    pub fn new(
        world: &'a mut World,
        stack: &'a mut Vec<ModuleId>,
        visited: &'a mut HashMap<ModuleId, VisitState>,
        module_id: ModuleId,
    ) -> Self {
        let mut info = ModuleInfo::new();

        if let Some(parent_id) = module_id.parent() {
            let bind = ModuleBind::path(parent_id.to_owned(), Vis::None);
            info.insert("super".into(), bind);
        }

        Self {
            module_id,
            info,
            world,
            stack,
            visited,
        }
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

        let submodule = match *value.get(tree) {
            node::ModuleExpr::Module(id) => {
                let submodule_id = ModuleId::new(self.module_id.clone(), self.module_id.path(), id);
                ModuleBind::module(submodule_id, vis)
            }
            node::ModuleExpr::Import(import_id) => {
                // Because imports are file based and strictly hierarchically (a module can always only have one parent)
                // We can just parse here and be confident that we didn't parse before.
                let file = match FileExplorer::new(tree, self.module_id.path())
                    .explore_import(import_id)
                {
                    Ok(file) => file,
                    Err(e) => return ControlFlow::Break(e.into()),
                };
                let path = file.source.path().to_owned();
                let id = file.tree.root_id();
                self.world.file_infos.insert(path.clone(), file);

                let submodule_id = ModuleId::new(self.module_id.clone(), path, id);
                ModuleBind::import(submodule_id, vis)
            }
            node::ModuleExpr::Path(path_id) => {
                let id = match ModuleExplorer::new(tree, &self.info, &self.world.module_infos)
                    .explore_path(path_id)
                {
                    Some(id) => id,
                    None => return ControlFlow::Break(ExploreError::ModuleNotFound),
                };

                ModuleBind::path(id, vis)
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
                return ControlFlow::Break(ExploreError::Cycle(self.module_id.path().to_owned()));
            }
            VisitState::Visited => (),
        }

        self.info.insert(name, submodule);
        ControlFlow::Continue(())
    }
}
