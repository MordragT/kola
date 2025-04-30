use log::debug;
use owo_colors::OwoColorize;
use std::collections::HashMap;

use kola_print::prelude::*;
use kola_syntax::prelude::*;
use kola_tree::prelude::*;

use crate::{
    error::{SourceDiagnostic, SourceReport},
    path::FilePath,
    source::Source,
};

pub type FileInfoTable = HashMap<FilePath, FileInfo>;

#[derive(Debug, Clone)]
pub struct FileInfo {
    pub source: Source,
    pub tree: Tree,
    pub spans: SpanInfo,
}

impl FileInfo {
    #[inline]
    pub fn span<T>(&self, id: Id<T>) -> Span
    where
        T: MetaCast<SyntaxPhase, Meta = Span>,
    {
        *self.spans.meta(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileParser {
    pub source: Source,
    pub options: PrintOptions,
}

impl FileParser {
    pub fn new(source: Source, options: PrintOptions) -> Self {
        Self { source, options }
    }

    pub fn try_parse(self) -> Result<FileInfo, SourceReport> {
        let Self { source, options } = self;

        let mut source_errors = Vec::new();

        debug!(
            "{} {:?}\n{}",
            "Source".bold().bright_white(),
            source.file_path(),
            source
        );

        let TokenizeResult { tokens, errors } = tokenize(source.as_str());
        source_errors.extend(errors.into_iter().map(SourceDiagnostic::from));

        let Some(tokens) = tokens else {
            return Err(SourceReport::new(source, source_errors));
        };

        debug!(
            "{} {:?}\n{}",
            "Tokens".bold().bright_white(),
            source.file_path(),
            TokenPrinter(&tokens).render(options)
        );

        let ParseResult {
            tree,
            spans,
            errors,
        } = parse(tokens.as_slice(), source.end_of_input());
        source_errors.extend(errors.into_iter().map(SourceDiagnostic::from));

        let Some(tree) = tree else {
            return Err(SourceReport::new(source, source_errors).into());
        };

        debug!(
            "{} {:?}\n{}",
            "Untyped Abstract Syntax Tree".bold().bright_white(),
            source.file_path(),
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
