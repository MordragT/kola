use kola_print::PrintOptions;
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_vfs::prelude::*;

use camino::Utf8Path;
use log::debug;
use miette::Result;
use owo_colors::OwoColorize;
use std::collections::HashMap;

use crate::{
    explorer::{ExploreReport, ModuleReports, explore},
    module::{ModuleInfo, ModuleInfoTable, ModulePath},
    typer::{self, TypeDecorator, TypeInfo, TypeInfoTable, Typer},
};

// TODO rename ModuleExplorer and FileExplorer to PathExplorer and ImportExplorer and move world
// to a own module with them as child files.

pub struct World {
    pub order: Vec<ModulePath>,
    pub file_infos: FileInfoTable,
    pub module_infos: ModuleInfoTable,
    pub module_parents: HashMap<ModulePath, ModulePath>,
    pub type_infos: TypeInfoTable,
    pub options: PrintOptions,
}

impl World {
    pub fn new(options: PrintOptions) -> Self {
        Self {
            order: Vec::new(),
            file_infos: HashMap::new(),
            module_infos: HashMap::new(),
            module_parents: HashMap::new(),
            type_infos: HashMap::new(),
            options,
        }
    }

    pub fn file_info(&self, id: &ModulePath) -> FileInfo {
        self.file_infos[&id.file_path].clone()
    }

    pub fn module_info(&self, id: &ModulePath) -> ModuleInfo {
        self.module_infos[id].clone()
    }

    pub fn type_info(&self, id: &ModulePath) -> TypeInfo {
        self.type_infos[id].clone()
    }
}

impl World {
    pub fn explore(
        path: impl AsRef<Utf8Path>,
        options: PrintOptions,
    ) -> Result<Self, ModuleReports> {
        let (file_path, import_path) = FilePath::open(path).unwrap();

        let ExploreReport {
            order,
            module_parents,
            file_infos,
            module_infos,
            reports,
        } = explore(file_path, import_path, options).unwrap();

        if !reports.0.is_empty() {
            return Err(reports);
        }

        Ok(Self {
            order,
            file_infos,
            module_infos,
            module_parents,
            type_infos: TypeInfoTable::new(),
            options,
        })
    }
}

impl World {
    pub fn type_check(&mut self) -> Result<&mut Self, typer::Error> {
        for module_path in &self.order {
            let module = self.module_info(module_path);
            let FileInfo {
                tree,
                spans,
                source,
            } = self.file_info(module_path);

            let types = Typer::new(
                module_path.clone(),
                module,
                spans.clone(),
                &self.module_infos,
                &self.module_parents,
                &self.type_infos,
            )
            .solve(&tree)?;
            self.type_infos
                .insert(module_path.to_owned(), types.clone());

            debug!(
                "{} {:?}\n{}",
                "Typed Abstract Syntax Tree".bold().bright_white(),
                source.file_path(),
                TreePrinter::new(&tree)
                    .with(LocDecorator(spans))
                    .with(TypeDecorator(types))
                    .render_at(module_path.id, self.options)
            );
        }

        Ok(self)
    }
}
