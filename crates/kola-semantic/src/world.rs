use kola_print::PrintOptions;
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use log::debug;
use miette::{Diagnostic, Result};
use owo_colors::OwoColorize;
use std::{collections::HashMap, io, ops::ControlFlow, path::Path};
use thiserror::Error;

use crate::{
    VisitState,
    file::{FileInfo, FileInfoTable, FileParser},
    module::{
        ModuleBind, ModuleExplorer, ModuleId, ModuleInfo, ModuleInfoBuilder, ModuleInfoTable,
        TypeBind, ValueBind,
    },
    typer::{self, TypeDecorator, TypeInfo, TypeInfoTable, Typer},
};

// TODO rename ModuleExplorer and FileExplorer to PathExplorer and ImportExplorer and move world
// to a own module with them as child files.

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
