use std::fmt::Display;

use chumsky::error::Rich;
use miette::{Diagnostic, LabeledSpan, SourceSpan};
use thiserror::Error;

use crate::errors::Errors;

use super::Source;

pub type SyntaxErrors = Errors<SyntaxError>;

#[derive(Error, Debug, Diagnostic)]
#[error("Parsing failed with the following errors:")]
#[diagnostic()]
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
#[error("{msg}")]
#[diagnostic(code(tyd_syntax::parser), url(docsrs), help("Please read the Book"))]
pub struct SyntaxError {
    pub msg: String,
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
        let msg = e.to_string();
        let trace = e
            .contexts()
            .map(|(label, span)| {
                LabeledSpan::new_with_span(Some(label.to_string()), span.into_range())
            })
            .collect();

        Self { span, msg, trace }
    }
}
