use std::{fmt::Display, sync::Arc};

use chumsky::error::Rich;
use miette::{Diagnostic, LabeledSpan, NamedSource, SourceSpan};
use thiserror::Error;

use super::Source;

pub type SyntaxResult<T> = Result<T, SyntaxReport>;

#[derive(Error, Debug, Diagnostic)]
#[error("Parsing failed with the following errors:")]
#[diagnostic()]
pub struct SyntaxReport {
    #[source_code]
    pub src: NamedSource<Arc<str>>,
    #[related]
    pub related: Vec<SyntaxError>,
}

impl SyntaxReport {
    pub fn from_rich<T>(src: &Source, errs: Vec<Rich<'_, T>>) -> Self
    where
        T: Display,
    {
        Self {
            src: src.named_source(),
            related: errs.into_iter().map(Into::into).collect(),
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("{msg}")]
#[diagnostic(code(tyd_syntax::parser), url(docsrs), help("Please read the Book"))]
pub struct SyntaxError {
    #[label("This here")]
    pub span: SourceSpan,
    pub msg: String,
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
