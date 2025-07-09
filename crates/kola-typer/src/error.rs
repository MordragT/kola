use kola_span::IntoDiagnostic;
use kola_utils::{errors::Errors, interner::StrKey, interner_ext::DisplayWithInterner};
use thiserror::Error;

use crate::types::{Kind, MonoType, PolyType, TypeVar};

#[derive(Debug, Clone, Error)]
pub enum TypeConversionError {
    #[error("Cannot convert `{0}` to a monomorphic type")]
    NotMonomorphic(PolyType),
}

impl IntoDiagnostic for TypeConversionError {}

pub type TypeErrors = Errors<TypeError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    Unbound(StrKey),
    Occurs(TypeVar),
    CannotMerge {
        lhs: MonoType,
        rhs: MonoType,
    },
    CannotMergeLabel {
        label: StrKey,
        lhs: MonoType,
        rhs: MonoType,
    },
    CannotUnify {
        expected: MonoType,
        actual: MonoType,
    },
    CannotUnifyLabel {
        label: StrKey,
        expected: MonoType,
        actual: MonoType,
        cause: TypeErrors,
    },
    CannotConstrain {
        expected: Kind,
        actual: MonoType,
    },
    ExtraLabel(StrKey),
    MissingLabel(StrKey),
}

impl DisplayWithInterner<str> for TypeError {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        interner: &kola_utils::interner::StrInterner,
    ) -> std::fmt::Result {
        match self {
            TypeError::Unbound(name) => writeln!(f, "Unbound: {}", interner[*name]),
            TypeError::Occurs(var) => writeln!(f, "Occurs: {}", var),
            TypeError::CannotMerge { lhs, rhs } => {
                writeln!(f, "Cannot Merge: Cannot merge `{}` with `{}`", lhs, rhs)
            }
            TypeError::CannotMergeLabel { label, lhs, rhs } => {
                writeln!(f, "Cannot Merge Label: {} : {} with {}", label, lhs, rhs)
            }
            TypeError::CannotUnify { expected, actual } => writeln!(
                f,
                "Cannot Unify: Expected `{}` but got `{}`",
                expected, actual
            ),
            TypeError::CannotUnifyLabel {
                label,
                expected,
                actual,
                cause,
            } => {
                writeln!(
                    f,
                    "Cannot Unify Label: {} : {} with {} because",
                    label, expected, actual
                )?;
                cause.fmt(f, interner)
            }
            TypeError::CannotConstrain { expected, actual } => {
                writeln!(f, "Cannot Constrain: {:?} {}", expected, actual)
            }
            TypeError::ExtraLabel(label) => {
                writeln!(f, "Extra Label: {}", interner[*label])
            }
            TypeError::MissingLabel(label) => {
                writeln!(f, "Missing Label: {}", interner[*label])
            }
        }
    }
}
