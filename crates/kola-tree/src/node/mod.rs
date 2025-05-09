use derive_more::{From, TryInto};
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, ops::Deref};

use kola_print::prelude::*;
use kola_utils::{StrKey, impl_try_as};

use crate::{id::Id, print::TreePrinter};

mod expr;
mod module;
mod pat;
mod ty;

pub use expr::*;
pub use module::*;
pub use pat::*;
pub use ty::*;

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[from(forward)]
pub struct Name(pub StrKey);

impl Name {
    #[inline]
    pub fn as_str_key(&self) -> &StrKey {
        &self.0
    }
}

impl Deref for Name {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<StrKey> for Name {
    #[inline]
    fn as_ref(&self) -> &StrKey {
        &self.0
    }
}

impl Borrow<StrKey> for Name {
    #[inline]
    fn borrow(&self) -> &StrKey {
        &self.0
    }
}

impl PartialEq<StrKey> for Name {
    #[inline]
    fn eq(&self, other: &StrKey) -> bool {
        self == other
    }
}

impl Printable<TreePrinter> for Name {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "Name".cyan().display_in(arena);

        let name = with
            .interner
            .get(self.0)
            .unwrap()
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

        #[derive(Clone, Debug, PartialEq, From, TryInto, Serialize, Deserialize)]
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
