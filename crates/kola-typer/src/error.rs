use std::fmt::Pointer;

use kola_utils::{
    display::DisplayWith,
    errors::Errors,
    interner::{StrInterner, StrKey},
};

use kola_types::{
    class::TypeClassError,
    kind::{Kind, KindError},
    types::{LabelOrVar, MergeError, MonoType, TypeVar},
};

pub type TypeErrors = Errors<TypeError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    Unbound(StrKey),
    Occurs(TypeVar),
    NotExistent(TypeVar),
    ExpectedRecord(MonoType),
    CannotMerge(MergeError),
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
    CannotConstrainClass(TypeClassError),
    CannotConstrainKind(KindError),
    KindMismatch {
        expected: Kind,
        actual: Kind,
    },
    ExtraLabel(LabelOrVar),
    MissingLabel(LabelOrVar),
}

impl DisplayWith<StrInterner> for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, interner: &StrInterner) -> std::fmt::Result {
        match self {
            TypeError::Unbound(name) => writeln!(f, "Unbound: {}", interner[*name]),
            TypeError::Occurs(var) => writeln!(f, "Occurs: {}", var),
            TypeError::NotExistent(var) => writeln!(f, "Not Existent: {}", var),
            TypeError::ExpectedRecord(mono) => {
                writeln!(f, "Expected Record: {}", mono)
            }
            TypeError::CannotMerge(e) => e.fmt(f),
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
            TypeError::CannotConstrainClass(e) => e.fmt(f),
            TypeError::CannotConstrainKind(e) => e.fmt(f),
            TypeError::KindMismatch { expected, actual } => {
                writeln!(f, "Kind Mismatch: Expected {} but got {}", expected, actual)
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

impl From<MergeError> for TypeError {
    fn from(e: MergeError) -> Self {
        TypeError::CannotMerge(e)
    }
}

impl From<TypeClassError> for TypeError {
    fn from(e: TypeClassError) -> Self {
        TypeError::CannotConstrainClass(e)
    }
}

impl From<KindError> for TypeError {
    fn from(e: KindError) -> Self {
        TypeError::CannotConstrainKind(e)
    }
}
