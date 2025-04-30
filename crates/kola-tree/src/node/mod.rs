use std::{borrow::Borrow, ops::Deref};

use crate::{id::Id, print::TreePrinter};

use derive_more::{From, TryInto};
use kola_print::prelude::*;
use kola_utils::impl_try_as;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

mod expr;
mod module;
mod pat;
mod ty;

pub use expr::*;
pub use module::*;
pub use pat::*;
pub use ty::*;

pub type Symbol = ecow::EcoString;

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[from(forward)]
pub struct Name(pub Symbol);

impl Name {
    #[inline]
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn as_symbol(&self) -> &Symbol {
        &self.0
    }
}

impl AsRef<str> for Name {
    #[inline]
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl Borrow<str> for Name {
    #[inline]
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}

impl PartialEq<str> for Name {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self == other
    }
}

impl Deref for Name {
    type Target = Symbol;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<Symbol> for Name {
    #[inline]
    fn as_ref(&self) -> &Symbol {
        &self.0
    }
}

impl Borrow<Symbol> for Name {
    #[inline]
    fn borrow(&self) -> &Symbol {
        &self.0
    }
}

impl PartialEq<Symbol> for Name {
    #[inline]
    fn eq(&self, other: &Symbol) -> bool {
        self == other
    }
}

impl Printable<TreePrinter> for Name {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "Name".cyan().display_in(arena);

        let name = self
            .0
            .yellow()
            .display_in(arena)
            .enclose_by(arena.just('"'), arena);

        let single = [arena.just(' '), name.clone()].concat_in(arena);
        let multi = [arena.newline(), name].concat_in(arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

macro_rules! define_nodes {
    ($($variant:ident),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum NodeKind {
            $($variant),*
        }

        #[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum NodeId {
            $($variant(Id<$variant>)),*
        }

        #[derive(Clone, Debug, PartialEq, From, TryInto)]
        pub enum Node {
            $($variant($variant)),*
        }

        impl Node {
            pub fn kind(&self) -> NodeKind {
                match self {
                    $(Self::$variant(_) => NodeKind::$variant,)*
                }
            }
        }

        impl_try_as!(
            Node,
            $($variant($variant)),*
        );
    }
}

define_nodes!(
    Name,
    // Patterns
    AnyPat,
    LiteralPat,
    IdentPat,
    RecordFieldPat,
    RecordPat,
    VariantCasePat,
    VariantPat,
    PatError,
    Pat,
    // Expressions
    LiteralExpr,
    PathExpr,
    ListExpr,
    RecordField,
    RecordExpr,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
    UnaryOp,
    UnaryExpr,
    BinaryOp,
    BinaryExpr,
    LetExpr,
    CaseBranch,
    CaseExpr,
    IfExpr,
    LambdaExpr,
    CallExpr,
    ExprError,
    Expr,
    // Types
    TypePath,
    TypeVar,
    RecordFieldType,
    RecordType,
    VariantCaseType,
    VariantType,
    FuncType,
    TypeApplication,
    TypeExpr,
    TypeError,
    Type,
    // Modules
    Vis,
    ValueBind,
    TypeBind,
    OpaqueTypeBind,
    ModuleBind,
    ModuleTypeBind,
    Bind,
    Module,
    ModulePath,
    ModuleImport,
    ModuleExpr,
    ValueSpec,
    OpaqueTypeKind,
    OpaqueTypeSpec,
    ModuleSpec,
    Spec,
    ModuleType
);
