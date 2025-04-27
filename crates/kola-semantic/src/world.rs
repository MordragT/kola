use kola_print::{PrintOptions, Printable};
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_utils::as_variant;
use miette::{Diagnostic, IntoDiagnostic, Result};
use owo_colors::OwoColorize;
use std::{
    collections::HashMap,
    ops::ControlFlow,
    path::{Path, PathBuf},
};
use thiserror::Error;

use crate::{
    module::{ModuleBind, ModuleId, ModuleInfo},
    typer::{self, TypeDecorator, TypeInfo, Typer},
};

// maybe this could use some DAG Graph for modelling dependencies which in turn allows for parallel module elaboration
pub struct World {
    pub sources: HashMap<PathBuf, Source>,
    pub trees: HashMap<PathBuf, Tree>,
    pub order: Vec<ModuleId>,
    pub span_infos: HashMap<PathBuf, SpanInfo>,
    pub module_infos: HashMap<ModuleId, ModuleInfo>,
    pub type_infos: HashMap<ModuleId, TypeInfo>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ExploreOptions {
    pub verbosity: ExploreVerbosity,
    pub print: PrintOptions,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ExploreVerbosity {
    pub debug: bool,
    pub source: bool,
    pub tokens: bool,
    pub tree: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum VisitState {
    Unvisited,
    Visiting,
    Visited,
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[error("Circular dependency detected at: {0}")]
struct CycleDetected(PathBuf);

impl World {
    pub fn new() -> Self {
        Self {
            order: Vec::new(),
            sources: HashMap::new(),
            trees: HashMap::new(),
            span_infos: HashMap::new(),
            module_infos: HashMap::new(),
            type_infos: HashMap::new(),
        }
    }

    pub fn source(&self, id: &ModuleId) -> Source {
        self.sources[&id.path].clone()
    }

    pub fn tree(&self, id: &ModuleId) -> Tree {
        self.trees[&id.path].clone()
    }

    pub fn spans(&self, id: &ModuleId) -> SpanInfo {
        self.span_infos[&id.path].clone()
    }

    pub fn module(&self, id: &ModuleId) -> &ModuleInfo {
        &self.module_infos[id]
    }

    pub fn types(&self, id: &ModuleId) -> TypeInfo {
        self.type_infos[id].clone()
    }
}

pub const SUPER_MODULE_PATH_SEGMENT: &'static str = "super";
pub const FILE_EXTENSION: &'static str = "kl";
// pub const PRIMARY_MODULE_FILE: &'static str = "mod.kl"; // Idea just use the same name for "primaries" e.g. a/b.kl is the primary and a/b/ is its importable dir

impl World {
    fn try_parse(&mut self, path: PathBuf, options: ExploreOptions) -> Result<Id<node::Module>> {
        let mut source_errors = Vec::new();

        if options.verbosity.debug {
            println!("\nVisiting {path:?}");
        }

        let source = Source::from_path(&path).into_diagnostic()?;
        self.sources.insert(path.clone(), source.clone());

        if options.verbosity.source {
            println!("{}", "Source".bold().bright_white());
            println!("{source}\n");
        }

        let TokenizeResult { tokens, mut errors } = tokenize(source.as_str());
        source_errors.append(&mut errors);

        let Some(tokens) = tokens else {
            return Err(SourceReport::new(source, source_errors).into());
        };

        if options.verbosity.tokens {
            println!("\n{}", "Tokens".bold().bright_white());
            TokenPrinter(&tokens).print(options.print);
        }

        let ParseResult {
            tree,
            spans,
            mut errors,
        } = parse(tokens, source.end_of_input());
        source_errors.append(&mut errors);
        self.span_infos.insert(path.clone(), spans.clone());

        let Some(tree) = tree else {
            return Err(SourceReport::new(source, source_errors).into());
        };
        self.trees.insert(path.clone(), tree.clone());

        let root = tree.root_id();

        if options.verbosity.tree {
            println!("\n{}", "Untyped Abstract Syntax Tree".bold().bright_white());
            TreePrinter::new(&tree)
                .with(SpanDecorator(spans.clone()))
                .print(options.print);
        }

        if source_errors.is_empty() {
            Ok(root)
        } else {
            Err(SourceReport::new(source, source_errors).into())
        }
    }

    // TODO only one level super (maybe just create super as a module inside the moduleinfo hashmap)
    // TODO module shoud only allow exports which are annotated with the "export" keyword (and prober error message should be returned)
    // TODO external packages could be provided also via HashMap<ModuleId, ModuleInfo>

    /// Topological DFS Algorithm
    pub fn explore(
        &mut self,
        path: impl AsRef<Path>,
        options: ExploreOptions,
    ) -> Result<&mut Self> {
        let path = path.as_ref().canonicalize().into_diagnostic()?;
        let id = self.try_parse(path.clone(), options)?;
        let module_id = ModuleId::root(path.clone(), id);

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

            let tree = self.tree(&module_id);
            let mut explorer =
                Explorer::new(self, &mut stack, &mut visited, options, module_id.clone());

            match module_id.id.visit_by(&mut explorer, &tree) {
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

struct Explorer<'a> {
    world: &'a mut World,
    stack: &'a mut Vec<ModuleId>,
    visited: &'a mut HashMap<ModuleId, VisitState>,
    options: ExploreOptions,
    module_id: ModuleId,
    info: ModuleInfo,
}

impl<'a> Explorer<'a> {
    pub fn new(
        world: &'a mut World,
        stack: &'a mut Vec<ModuleId>,
        visited: &'a mut HashMap<ModuleId, VisitState>,
        options: ExploreOptions,
        module_id: ModuleId,
    ) -> Self {
        Self {
            options,
            module_id,
            info: ModuleInfo::new(),
            world,
            stack,
            visited,
        }
    }
}

impl<'a, T: TreeAccess> Visitor<T> for Explorer<'a> {
    type BreakValue = CycleDetected;

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let node::ModuleBind {
            vis,
            name,
            ty,
            value,
        } = *id.get(tree);

        let vis = *vis.get(tree);
        let name = name.get(tree).0.clone();

        let submodule = match *value.get(tree) {
            node::ModuleExpr::Module(id) => {
                let submodule_id =
                    ModuleId::new(self.module_id.clone(), self.module_id.path.clone(), id);
                ModuleBind::module(submodule_id, vis)
            }
            node::ModuleExpr::Import(import_id) => {
                // Because imports are file based and strictly hierarchically (a module can always only have one parent)
                // We can just parse here and be confident that we didn't parse before.
                let path = self
                    .module_id
                    .work_dir()
                    .join(import_id.get(tree).0.get(tree).as_str())
                    .with_extension(FILE_EXTENSION);

                let id = self.world.try_parse(path.clone(), self.options).unwrap(); // TODO handle error
                let submodule_id = ModuleId::new(self.module_id.clone(), path, id);
                ModuleBind::import(submodule_id, vis)
            }
            node::ModuleExpr::Path(path_id) => {
                let mut segments = path_id.get(tree).0.iter();

                let first = segments.next().unwrap().get(tree);

                let mut id = if first.as_str() == SUPER_MODULE_PATH_SEGMENT {
                    // TODO just create a super module for every child module by default
                    self.module_id.parent.as_ref().unwrap() // TODO error
                } else {
                    &self.info.get(&first.0).unwrap().id // TODO error
                };

                for segment in segments {
                    id = &self.world.module(&id).get(&segment.get(tree).0).unwrap().id; // TODO error
                }

                ModuleBind::path(id.clone(), vis)
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
                return ControlFlow::Break(CycleDetected(self.module_id.path.clone()));
            }
            VisitState::Visited => (),
        }

        self.info.insert(name, submodule);
        ControlFlow::Continue(())
    }
}

impl World {
    pub fn infer(
        &mut self,
        verbose: bool,
        options: PrintOptions,
    ) -> Result<&mut Self, typer::Error> {
        for module_id in &self.order {
            let tree = self.tree(module_id);
            let root = tree.root_id();
            let spans = self.spans(module_id);

            let types = Typer::new(&tree, spans.clone()).solve(root, &tree)?;
            self.type_infos.insert(module_id.to_owned(), types.clone());

            if verbose {
                println!("\n{}", "Typed Abstract Syntax Tree".bold().bright_white());
                TreePrinter::new(&tree)
                    .with(SpanDecorator(spans))
                    .with(TypeDecorator(types))
                    .print_at(module_id.id, options);
            }
        }

        Ok(self)
    }
}

// pub fn resolve_module_path(
//     current_path: impl Into<PathBuf>,
//     id: Id<node::ModulePath>,
//     tree: &impl TreeAccess,
// ) -> PathBuf {
//     let current_path = current_path.into();

//     let file_name = current_path.file_name().unwrap();

//     let mut path = if file_name == PRIMARY_MODULE_FILE {
//         current_path.parent().unwrap().to_owned()
//     } else {
//         current_path
//     };

//     let module_path = id.get(tree);

//     for (i, &id) in module_path.0.iter().enumerate() {
//         let segment = id.get(tree);

//         if segment == SUPER_MODULE_PATH_SEGMENT && i == 0 {
//             path = path.parent().unwrap().to_owned();
//         } else {
//             path.push(segment.as_str());
//         }
//     }

//     if path.is_dir() {
//         path.push(PRIMARY_MODULE_FILE);
//     } else {
//         path.add_extension(FILE_EXTENSION);
//     }

//     path
// }
