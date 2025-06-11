use derive_more::{From, TryInto};
use serde::{Deserialize, Serialize};

use kola_utils::impl_try_as;

use crate::id::Id;

mod expr;
mod module;
mod namespace;
mod pat;
mod ty;

pub use expr::*;
pub use module::*;
pub use namespace::*;
pub use pat::*;
pub use ty::*;

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

        impl NodeId {
            pub(crate) fn from_usize(id: usize, kind: NodeKind) -> Self {
                match kind{
                    $(NodeKind::$variant => Self::$variant(Id::unchecked_from_usize(id)),)*
                }
            }

            pub fn kind(&self) -> NodeKind {
                match self {
                    $(Self::$variant(_) => NodeKind::$variant,)*
                }
            }
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
    ModuleName,
    TypeName,
    ValueName,
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
