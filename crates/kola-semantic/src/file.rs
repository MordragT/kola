use std::{
    io,
    path::{Path, PathBuf},
};

use kola_print::prelude::*;
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use log::debug;
use miette::Diagnostic;
use owo_colors::OwoColorize;
use thiserror::Error;

pub type FileResult = Result<FileInfo, FileError>;

#[derive(Debug, Error, Diagnostic)]
pub enum FileError {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[diagnostic(transparent)]
    #[error(transparent)]
    Source(#[from] SourceReport),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileExplorer<'a, T> {
    pub tree: &'a T,
    pub path: &'a Path,
}

impl<'a, T> FileExplorer<'a, T>
where
    T: TreeAccess,
{
    pub const EXTENSION: &'static str = "kl";

    pub fn new(tree: &'a T, path: &'a Path) -> Self {
        Self { tree, path }
    }

    pub fn module_dir(self) -> PathBuf {
        let stem = self.path.file_stem().unwrap();
        self.path.parent().unwrap().join(stem)
    }

    pub fn import_path_of(self, id: Id<node::ModuleImport>) -> PathBuf {
        let name = id.get(self.tree).0.get(self.tree);

        self.module_dir()
            .join(name.as_str())
            .with_extension(Self::EXTENSION)
    }

    // TODO errors
    pub fn explore_import(self, id: Id<node::ModuleImport>) -> FileResult {
        let path = self.import_path_of(id);

        let source = Source::from_path(&path)?;
        let file = FileParser::new(source).try_parse()?;
        Ok(file)
    }
}

#[derive(Debug, Clone)]
pub struct FileInfo {
    pub source: Source,
    pub tree: Tree,
    pub spans: SpanInfo,
}

impl FileInfo {
    pub fn explore(&self) -> FileExplorer<'_, Tree> {
        FileExplorer {
            tree: &self.tree,
            path: self.source.path(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileParser {
    pub source: Source,
    pub options: PrintOptions,
}

impl FileParser {
    pub fn new(source: Source) -> Self {
        Self {
            source,
            options: Default::default(),
        }
    }

    pub fn with(source: Source, options: PrintOptions) -> Self {
        Self { source, options }
    }

    pub fn try_parse(self) -> Result<FileInfo, SourceReport> {
        let Self { source, options } = self;

        let mut source_errors = Vec::new();

        debug!("{}\n{}", "Source".bold().bright_white(), source);

        let TokenizeResult { tokens, mut errors } = tokenize(source.as_str());
        source_errors.append(&mut errors);

        let Some(tokens) = tokens else {
            return Err(SourceReport::new(source, source_errors));
        };

        debug!(
            "{}\n{}",
            "Tokens".bold().bright_white(),
            TokenPrinter(&tokens).render(options)
        );

        let ParseResult {
            tree,
            spans,
            mut errors,
        } = parse(tokens, source.end_of_input());
        source_errors.append(&mut errors);

        let Some(tree) = tree else {
            return Err(SourceReport::new(source, source_errors).into());
        };

        debug!(
            "{}\n{}",
            "Untyped Abstract Syntax Tree".bold().bright_white(),
            TreePrinter::new(&tree)
                .with(SpanDecorator(spans.clone()))
                .render(options)
        );

        if source_errors.is_empty() {
            Ok(FileInfo {
                source,
                tree,
                spans,
            })
        } else {
            Err(SourceReport::new(source, source_errors).into())
        }
    }
}
