use std::{
    io,
    path::{Path, PathBuf},
};

use kola_print::prelude::*;
use kola_syntax::prelude::*;
use kola_tree::prelude::*;
use log::debug;
use owo_colors::OwoColorize;

#[derive(Debug, Clone)]
pub struct FileExplorer {
    pub source: Source,
    pub tree: Tree,
    pub spans: SpanInfo,
}

impl From<FileInfo> for FileExplorer {
    fn from(info: FileInfo) -> Self {
        let FileInfo {
            source,
            tree,
            spans,
        } = info;

        Self {
            source,
            tree,
            spans,
        }
    }
}

impl FileExplorer {
    pub const EXTENSION: &'static str = "kl";

    pub fn span<T>(&self, id: Id<T>) -> Span
    where
        T: MetaCast<SyntaxPhase, Meta = Span>,
    {
        self.spans.get(id).inner_copied()
    }

    pub fn module_dir(&self) -> PathBuf {
        let path = self.source.path();

        let stem = path.file_stem().unwrap();
        path.parent().unwrap().join(stem)
    }

    pub fn import_path_of(&self, id: Id<node::ModuleImport>) -> PathBuf {
        let name = id.get(&self.tree).0.get(&self.tree);

        self.module_dir()
            .join(name.as_str())
            .with_extension(Self::EXTENSION)
    }

    pub fn import(&self, id: Id<node::ModuleImport>) -> Result<FileInfo, SourceReport> {
        let path = self.import_path_of(id);

        let source = Source::from_path(&path).map_err(|e| {
            SourceDiagnostic::error(self.span(id), e.to_string()).report(self.source.clone())
        })?;
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
    pub fn explore(&self) -> FileExplorer {
        FileExplorer {
            source: self.source.clone(),
            tree: self.tree.clone(),
            spans: self.spans.clone(),
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
