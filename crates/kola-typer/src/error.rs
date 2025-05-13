use kola_syntax::prelude::*;
use kola_utils::errors::Errors;
use kola_vfs::diag::{IntoSourceDiagnostic, SourceDiagnostic};
use miette::Diagnostic;
use thiserror::Error;

use crate::types::{Kind, MonoType, TypeVar};

pub type SemanticErrors = Errors<SemanticError>;

#[derive(Debug, Clone, Error, Diagnostic, PartialEq, Eq)]
pub enum SemanticError {
    #[error("Unbound: {0}")]
    Unbound(String),
    #[error("Occurs: {0}")]
    Occurs(TypeVar),
    #[error("Cannot Unify: Expected `{expected}` but got `{actual}`")]
    CannotUnify {
        expected: MonoType,
        actual: MonoType,
    },
    #[error("Cannot Unify Label: {label} : {expected} with {actual} because\n{cause}")]
    CannotUnifyLabel {
        label: String,
        expected: MonoType,
        actual: MonoType,
        cause: SemanticErrors,
    },
    #[error("Cannot Constrain: {expected:?} {actual}")]
    CannotConstrain { expected: Kind, actual: MonoType },
    #[error("Extra Label: {0}")]
    ExtraLabel(String),
    #[error("Missing Label: {0}")]
    MissingLabel(String),
}

impl SemanticError {
    pub fn with(self, span: Span) -> Spanned<SemanticErrors> {
        let related = Errors::from(vec![self]);
        (related, span)
    }
}

impl IntoSourceDiagnostic for SemanticError {
    fn into_source_diagnostic(self, span: Span) -> SourceDiagnostic {
        // TODO maybe improve ?
        match self {
            other => SourceDiagnostic::error(span, other.to_string()),
        }
    }
}
