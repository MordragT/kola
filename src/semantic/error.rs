use std::sync::Arc;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::syntax::{ast::Ident, Span};

use super::{
    types::{MonoType, TypeVar},
    Kind,
};

#[derive(Debug, Clone, Error, Diagnostic, PartialEq, Eq)]
pub enum InferError {
    #[error("Unbound: {0}")]
    Unbound(Ident),
    #[error("Occurs: {0}")]
    Occurs(TypeVar),
    #[error("Cannot Unify: Expected `{expected}` but got `{actual}`")]
    CannotUnify {
        expected: MonoType,
        actual: MonoType,
    },
    #[error("Cannot Unify Label: {label} : {expected} with {actual} because {cause:?}")]
    CannotUnifyLabel {
        label: Ident,
        expected: MonoType,
        actual: MonoType,
        cause: Vec<InferError>,
    },
    #[error("Cannot Constrain: {expected:?} {actual}")]
    CannotConstrain { expected: Kind, actual: MonoType },
    #[error("Extra Label: {0}")]
    ExtraLabel(Ident),
    #[error("Missing Label: {0}")]
    MissingLabel(Ident),
}

impl InferError {
    pub fn with(self, span: Span, source: NamedSource<Arc<str>>) -> InferReport {
        InferReport::new(vec![self], span, source)
    }
}

#[derive(Debug, Clone, Error, Diagnostic, PartialEq, Eq)]
#[error("Inference failed with:")]
pub struct InferReport {
    #[source_code]
    pub src: NamedSource<Arc<str>>,
    #[label("This here")]
    pub span: SourceSpan,
    #[related]
    pub related: Vec<InferError>,
}

impl InferReport {
    pub fn new(related: Vec<InferError>, span: Span, source: NamedSource<Arc<str>>) -> Self {
        Self {
            span: SourceSpan::from(span.into_range()),
            related,
            src: source,
        }
    }
}
