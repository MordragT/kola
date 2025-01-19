use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    errors::Errors,
    source::Source,
    syntax::{ast::Symbol, Span, Spanned},
};

use super::types::{Kind, MonoType, TypeVar};

pub type SemanticErrors = Errors<SemanticError>;

#[derive(Debug, Clone, Error, Diagnostic, PartialEq, Eq)]
pub enum SemanticError {
    #[error("Unbound: {0}")]
    Unbound(Symbol),
    #[error("Occurs: {0}")]
    Occurs(TypeVar),
    #[error("Cannot Unify: Expected `{expected}` but got `{actual}`")]
    CannotUnify {
        expected: MonoType,
        actual: MonoType,
    },
    #[error("Cannot Unify Label: {label} : {expected} with {actual} because {cause:?}")]
    CannotUnifyLabel {
        label: Symbol,
        expected: MonoType,
        actual: MonoType,
        cause: Vec<SemanticError>,
    },
    #[error("Cannot Constrain: {expected:?} {actual}")]
    CannotConstrain { expected: Kind, actual: MonoType },
    #[error("Extra Label: {0}")]
    ExtraLabel(Symbol),
    #[error("Missing Label: {0}")]
    MissingLabel(Symbol),
}

impl SemanticError {
    pub fn with(self, span: Span) -> Spanned<SemanticErrors> {
        let related = Errors::from(vec![self]);
        (related, span)
    }
}

#[derive(Debug, Clone, Error, Diagnostic, PartialEq, Eq)]
#[error("Inference failed with:")]
pub struct SemanticReport {
    #[source_code]
    pub src: Source,
    #[label("This here")]
    pub span: SourceSpan,
    #[related]
    pub related: SemanticErrors,
}

impl SemanticReport {
    pub fn new(source: Source, span: Span, related: SemanticErrors) -> Self {
        Self {
            src: source,
            span: SourceSpan::from(span.into_range()),
            related,
        }
    }
}
