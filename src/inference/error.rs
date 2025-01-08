use thiserror::Error;

use crate::syntax::ast::Ident;

use super::{MonoType, TypeVar};

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum InferError {
    #[error("Unbound: {0}")]
    Unbound(Ident),
    #[error("Occurs: {0}")]
    Occurs(TypeVar),
    #[error("Failure: {0:?} {1:?}")]
    Failure(MonoType, MonoType),
}

pub type InferResult<T> = Result<T, InferError>;
