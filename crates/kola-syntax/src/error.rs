use std::fmt::Display;

use chumsky::error::Rich;
use kola_utils::Errors;
use miette::{Diagnostic, LabeledSpan, SourceSpan};
use thiserror::Error;

use crate::source::Source;

pub type SyntaxErrors = Errors<SyntaxError>;

#[derive(Error, Debug, Diagnostic)]
#[error("Reporting Syntax Errors:")]
pub struct SyntaxReport {
    #[source_code]
    pub src: Source,
    #[related]
    pub related: SyntaxErrors,
}

impl SyntaxReport {
    pub fn new(source: Source, related: SyntaxErrors) -> Self {
        Self {
            src: source,
            related,
        }
    }

    pub fn from_rich<T>(source: Source, related: Vec<Rich<'_, T>>) -> Self
    where
        T: Display,
    {
        Self {
            src: source,
            related: related.into_iter().map(Into::into).collect(),
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
#[diagnostic(code(syntax), url(docsrs), help("Please read the Book"))]
#[error("{reason}")]
pub struct SyntaxError {
    pub reason: String,
    #[label("This here")]
    pub span: SourceSpan,
    #[label(collection, "Related to this")]
    pub trace: Vec<LabeledSpan>,
}

impl<'src, T> From<Rich<'src, T>> for SyntaxError
where
    T: Display,
{
    fn from(e: Rich<'src, T>) -> Self {
        let span = SourceSpan::from(e.span().into_range());
        let reason = e.to_string();
        let trace = e
            .contexts()
            .map(|(label, span)| {
                LabeledSpan::new_with_span(Some(label.to_string()), span.into_range())
            })
            .collect();

        Self {
            span,
            reason,
            trace,
        }
    }
}
