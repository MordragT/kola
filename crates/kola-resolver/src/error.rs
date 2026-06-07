use std::hash::Hash;

use kola_span::{Diagnostic, Loc, Located};
use kola_tree::node::NamespaceKind;

pub fn name_collision(
    this: Located<NamespaceKind>,
    other: Located<NamespaceKind>,
) -> NameCollision {
    use NamespaceKind::*;

    let help = match (this.0, other.0) {
        (ModuleType, ModuleType) => {
            "Module type bindings must have distinct names from Module type bindings."
        }
        (Module, Module) => "Module bindings must have distinct names from Module bindings.",
        (Module, Value) => "Module bindings must have distinct names from Value bindings.",
        (Module, Type) => "Module bindings must have distinct names from Type bindings.",
        (Value, Module) => "Value bindings must have distinct names from Module bindings.",
        (Value, Value) => "Value bindings must have distinct names from Value bindings.",
        (Value, Type) => "Value bindings must have distinct names from Type bindings.",
        (Type, Module) => "Type bindings must have distinct names from Module bindings.",
        (Type, Value) => "Type bindings must have distinct names from Value bindings.",
        (Type, Type) => "Type bindings must have distinct names from Type bindings.",
        (_, _) => "Bindings must have distinct names.",
    };

    match this.0 {
        ModuleType => NameCollision::module_type_bind(this.1, other.1, help),
        Module => NameCollision::module_bind(this.1, other.1, help),
        Value => NameCollision::value_bind(this.1, other.1, help),
        Type => NameCollision::type_bind(this.1, other.1, help),
        Functor => NameCollision::functor_bind(this.1, other.1, help),
        _ => unreachable!(),
    }
}

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
    EffectTypeBind {
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
    pub const fn effect_type_bind(span: Loc, other: Loc, help: &'static str) -> Self {
        NameCollision::EffectTypeBind { span, other, help }
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
            NameCollision::EffectTypeBind { span, other, help } => Diagnostic::error(
                span,
                "An effect type bind with the same name was defined before",
            )
            .with_trace([("This effect type bind here".to_owned(), other)])
            .with_help(help),
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
