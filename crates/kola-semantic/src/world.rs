use kola_print::{PrintOptions, Printable};
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use kola_utils::as_variant;
use miette::{Diagnostic, IntoDiagnostic, Result};
use owo_colors::OwoColorize;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
use thiserror::Error;

// maybe this could use some DAG Graph for modelling dependencies which in turn allows for parallel module elaboration
pub struct World {
    pub order: Vec<PathBuf>,
    pub sources: HashMap<PathBuf, Source>,
    pub trees: HashMap<PathBuf, Tree>,
    pub span_table: HashMap<PathBuf, SpanMetadata>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DiscoverOptions {
    pub verbose: DiscoverVerboseOptions,
    pub print: PrintOptions,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DiscoverVerboseOptions {
    pub source: bool,
    pub tokens: bool,
    pub tree: bool,
    pub typed_tree: bool,
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
    pub fn discover(path: impl AsRef<Path>, options: DiscoverOptions) -> Result<Self> {
        let path = path.as_ref().canonicalize().into_diagnostic()?;

        let mut stack = vec![path];
        let mut order = Vec::new();
        let mut visited = HashMap::new();
        let mut sources = HashMap::new();
        let mut source_errors = Vec::new();
        let mut trees = HashMap::new();
        let mut span_table = HashMap::new();

        while let Some(path) = stack.last().cloned() {
            println!("Processing {path:?}");

            match visited.get(&path).copied().unwrap_or(VisitState::Unvisited) {
                VisitState::Unvisited => {
                    visited.insert(path.clone(), VisitState::Visiting);
                }
                VisitState::Visiting => {
                    visited.insert(path.clone(), VisitState::Visited);
                    order.push(path.clone());
                    stack.pop();
                    continue;
                }
                VisitState::Visited => {
                    stack.pop();
                    continue;
                }
            }

            let source = Source::from_path(&path).into_diagnostic()?;
            sources.insert(path.clone(), source.clone());

            if options.verbose.source {
                println!("{}", "Source".bold().bright_white());
                println!("{source}\n");
            }

            let TokenizeResult { tokens, mut errors } = tokenize(source.as_str());
            source_errors.append(&mut errors);

            let Some(tokens) = tokens else {
                return Err(SourceReport::new(source, source_errors).into());
            };

            if options.verbose.tokens {
                println!("\n{}", "Tokens".bold().bright_white());
                TokenPrinter(&tokens).print(options.print);
            }

            let ParseResult {
                tree,
                spans,
                mut errors,
            } = parse(tokens, source.end_of_input());
            source_errors.append(&mut errors);
            span_table.insert(path.clone(), spans.clone());

            let Some(tree) = tree else {
                return Err(SourceReport::new(source, source_errors).into());
            };
            trees.insert(path.clone(), tree.clone());

            if options.verbose.tree {
                println!("\n{}", "Untyped Abstract Syntax Tree".bold().bright_white());
                TreePrinter::new(&tree)
                    .with(SpanDecorator(spans.clone()))
                    .print(options.print);
            }

            if !source_errors.is_empty() {
                return Err(SourceReport::new(source, source_errors).into());
            }

            for node::ModuleImport(id) in tree
                .iter_nodes()
                .filter_map(|node| as_variant!(node, Node::ModuleImport))
            {
                let next = resolve_module_path(&path, *id, &tree);

                match visited.get(&next).copied().unwrap_or(VisitState::Unvisited) {
                    VisitState::Unvisited => stack.push(next),
                    VisitState::Visiting => return Err(CycleDetected(next).into()),
                    VisitState::Visited => (),
                }
            }
        }

        order.reverse();

        Ok(Self {
            order,
            sources,
            trees,
            span_table,
        })
    }
}

pub const SUPER_MODULE_PATH_SEGMENT: &'static str = "super";
pub const FILE_EXTENSION: &'static str = "kl";
pub const PRIMARY_MODULE_FILE: &'static str = "mod.kl";

pub fn resolve_module_path(
    current_path: impl Into<PathBuf>,
    id: Id<node::ModulePath>,
    tree: &impl TreeAccess,
) -> PathBuf {
    let current_path = current_path.into();

    let file_name = current_path.file_name().unwrap();

    let mut path = if file_name == PRIMARY_MODULE_FILE {
        current_path.parent().unwrap().to_owned()
    } else {
        current_path
    };

    let module_path = id.get(tree);

    for (i, &id) in module_path.0.iter().enumerate() {
        let segment = id.get(tree);

        if segment == SUPER_MODULE_PATH_SEGMENT && i == 0 {
            path = path.parent().unwrap().to_owned();
        } else {
            path.push(segment.as_str());
        }
    }

    if path.is_dir() {
        path.push(PRIMARY_MODULE_FILE);
    } else {
        path.add_extension(FILE_EXTENSION);
    }

    path
}
