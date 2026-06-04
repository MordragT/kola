use derive_more::Display;
use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Every type in the system has a kind, which is a classification of the type.
/// Used for kind preserving unification (see Extensible Records with Scoped Labels)
#[derive(
    Debug,
    Display,
    Default,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum Kind {
    #[default]
    Type,
    Row,
    Label,
}

#[derive(Debug, Clone, Error, PartialEq, Eq)]
#[error("Kind Mismatch: Expected {expected} but got {actual}")]
pub struct KindError {
    pub expected: Kind,
    pub actual: Kind,
}

pub trait CheckKind {
    fn kind(&self) -> Kind;

    fn check_kind(&self, with: Kind) -> Result<(), KindError> {
        let kind = self.kind();

        if with == kind {
            Ok(())
        } else {
            Err(KindError {
                expected: with,
                actual: kind,
            })
        }
    }
}
