use kola_span::IntoDiagnostic;
use kola_utils::errors::Errors;
use thiserror::Error;

use crate::types::{Kind, MonoType, TypeVar};

pub type SemanticErrors = Errors<SemanticError>;

#[derive(Debug, Clone, Error, PartialEq, Eq)]
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

impl IntoDiagnostic for SemanticError {}
