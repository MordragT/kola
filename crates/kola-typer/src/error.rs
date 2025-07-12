use kola_span::IntoDiagnostic;
use kola_utils::{errors::Errors, interner::StrKey, interner_ext::DisplayWithInterner};
use thiserror::Error;

use crate::types::{LabelOrVar, MonoType, PolyType, TypeClass, TypeVar};

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
    NotExistent(TypeVar),
    CannotMerge {
        lhs: MonoType,
        rhs: MonoType,
    },
    CannotMergeLabel {
        label: LabelOrVar,
        lhs: MonoType,
        rhs: MonoType,
    },
    CannotUnify {
        expected: MonoType,
        actual: MonoType,
    },
    CannotUnifyLabel {
        label: LabelOrVar,
        expected: MonoType,
        actual: MonoType,
        cause: TypeErrors,
    },
    CannotConstrain {
        expected: TypeClass,
        actual: MonoType,
    },
    ExtraLabel(LabelOrVar),
    MissingLabel(LabelOrVar),
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
            TypeError::NotExistent(var) => writeln!(f, "Not Existent: {}", var),
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
                writeln!(f, "Extra Label: ")?;
                label.fmt(f, interner)
            }
            TypeError::MissingLabel(label) => {
                writeln!(f, "Missing Label: ")?;
                label.fmt(f, interner)
            }
        }
    }
}
