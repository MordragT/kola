use std::hash::Hash;

use kola_span::{Diagnostic, Loc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameCollision {
    LocalBind {
        span: Loc,
        other: Loc,
        help: &'static str,
    },
    ValueBind {
        span: Loc,
        other: Loc,
        help: &'static str,
    },
    TypeBind {
        span: Loc,
        other: Loc,
        help: &'static str,
    },
    ModuleBind {
        span: Loc,
        other: Loc,
        help: &'static str,
    },
    ModuleTypeBind {
        span: Loc,
        other: Loc,
        help: &'static str,
    },
    FunctorBind {
        span: Loc,
        other: Loc,
        help: &'static str,
    },
}

impl NameCollision {
    #[inline]
    pub const fn local_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::LocalBind { span, other, help }
    }

    #[inline]
    pub const fn value_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::ValueBind { span, other, help }
    }

    #[inline]
    pub const fn type_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::TypeBind { span, other, help }
    }

    #[inline]
    pub const fn module_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::ModuleBind { span, other, help }
    }

    #[inline]
    pub const fn module_type_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::ModuleTypeBind { span, other, help }
    }

    #[inline]
    pub const fn functor_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::FunctorBind { span, other, help }
    }
}

impl From<NameCollision> for Diagnostic {
    fn from(value: NameCollision) -> Self {
        match value {
            NameCollision::LocalBind { span, other, help } => {
                Diagnostic::error(span, "A local bind with the same name was defined before")
                    .with_trace([("This local bind here".to_owned(), other)])
                    .with_help(help)
            }
            NameCollision::ValueBind { span, other, help } => {
                Diagnostic::error(span, "A value bind with the same name was defined before")
                    .with_trace([("This value bind here".to_owned(), other)])
                    .with_help(help)
            }
            NameCollision::TypeBind { span, other, help } => {
                Diagnostic::error(span, "A type bind with the same name was defined before")
                    .with_trace([("This type bind here".to_owned(), other)])
                    .with_help(help)
            }
            NameCollision::ModuleBind { span, other, help } => {
                Diagnostic::error(span, "A module bind with the same name was defined before")
                    .with_trace([("This module bind here".to_owned(), other)])
                    .with_help(help)
            }
            NameCollision::ModuleTypeBind { span, other, help } => Diagnostic::error(
                span,
                "A module type bind with the same name was defined before",
            )
            .with_trace([("This module type bind here".to_owned(), other)])
            .with_help(help),
            NameCollision::FunctorBind { span, other, help } => {
                Diagnostic::error(span, "A functor bind with the same name was defined before")
                    .with_trace([("This functor bind here".to_owned(), other)])
                    .with_help(help)
            }
        }
    }
}
