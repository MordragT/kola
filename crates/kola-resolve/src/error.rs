use std::hash::Hash;

use kola_span::{Diagnostic, Loc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameCollision {
    ValueBind {
        span: Loc,
        other: Loc,
        help: &'static str,
    },
    ModuleBind {
        span: Loc,
        other: Loc,
        help: &'static str,
    },
    TypeBind {
        span: Loc,
        other: Loc,
        help: &'static str,
    },
    LocalBind {
        span: Loc,
        other: Loc,
        help: &'static str,
    },
}

impl NameCollision {
    pub const fn value_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::ValueBind { span, other, help }
    }
    pub const fn module_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::ModuleBind { span, other, help }
    }
    pub const fn type_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::TypeBind { span, other, help }
    }

    pub const fn local_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::LocalBind { span, other, help }
    }
}

impl From<NameCollision> for Diagnostic {
    fn from(value: NameCollision) -> Self {
        match value {
            NameCollision::ValueBind { span, other, help } => {
                Diagnostic::error(span, "A value bind with the same name was defined before")
                    .with_trace([("This value bind here".to_owned(), other)])
                    .with_help(help)
            }
            NameCollision::ModuleBind { span, other, help } => {
                Diagnostic::error(span, "A module bind with the same name was defined before")
                    .with_trace([("This module bind here".to_owned(), other)])
                    .with_help(help)
            }
            NameCollision::TypeBind { span, other, help } => {
                Diagnostic::error(span, "A type bind with the same name was defined before")
                    .with_trace([("This type bind here".to_owned(), other)])
                    .with_help(help)
            }
            NameCollision::LocalBind { span, other, help } => {
                Diagnostic::error(span, "A local bind with the same name was defined before")
                    .with_trace([("This local bind here".to_owned(), other)])
                    .with_help(help)
            }
        }
    }
}
