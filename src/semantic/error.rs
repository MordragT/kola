use thiserror::Error;

use crate::syntax::ast::Ident;

use super::{
    types::{MonoType, TypeVar},
    Kind,
};

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum InferError {
    #[error("Unbound: {0}")]
    Unbound(Ident),
    #[error("Occurs: {0}")]
    Occurs(TypeVar),
    #[error("Cannot Unify: {expected} {actual}")]
    CannotUnify {
        expected: MonoType,
        actual: MonoType,
    },
    #[error("Cannot Unify Label: {label} : {expected} with {actual} because")]
    CannotUnifyLabel {
        label: Ident,
        expected: MonoType,
        actual: MonoType,
        // cause: Box<dyn std::error::Error>,
    },
    #[error("Cannot Constrain: {expected:?} {actual}")]
    CannotConstrain { expected: Kind, actual: MonoType },
}

#[derive(Debug, Clone, Error, PartialEq, Eq)]
#[error("Inference failed with:")]
pub struct InferErrors {
    pub related: Vec<InferError>,
}

impl From<InferError> for InferErrors {
    fn from(value: InferError) -> Self {
        Self {
            related: vec![value],
        }
    }
}

pub type InferResult<T> = Result<T, InferError>;
