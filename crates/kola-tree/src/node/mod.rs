use derive_more::{From, TryInto};
use serde::{Deserialize, Serialize};
use std::mem;

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
            pub const BITS: usize = mem::size_of::<Self>();

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
    EffectName,
    FunctorName,
    ModuleTypeName,
    ModuleName,
    TypeName,
    ValueName,
    // Patterns
    AnyPat,
    LiteralPat,
    BindPat,
    ListElPat,
    ListPat,
    RecordFieldPat,
    RecordPat,
    VariantTagPat,
    VariantPat,
    PatError,
    Pat,
    // Expressions
    LiteralExpr,
    ListExpr,
    RecordField,
    RecordExpr,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
    RecordMergeExpr,
    FieldPath,
    QualifiedExpr,
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
    HandlerClause,
    HandleExpr,
    DoExpr,
    TagExpr,
    TypeWitnessExpr,
    ExprError,
    Expr,
    // Types
    QualifiedEffectType,
    EffectOpType,
    EffectRowType,
    EffectType,
    QualifiedType,
    TypeVar,
    Label,
    RecordFieldType,
    RecordType,
    TagType,
    VariantType,
    FuncType,
    TypeApplication,
    CompType,
    Type,
    TypeError,
    Kind,
    TypeVarBind,
    ForallBinder,
    TypeScheme,
    // Modules
    Vis,
    ValueBind,
    TypeBind,
    OpaqueTypeBind,
    EffectTypeBind,
    ModuleBind,
    ModuleTypeBind,
    FunctorBind,
    Bind,
    ModuleError,
    Module,
    ModulePath,
    ModuleImport,
    FunctorApp,
    ModuleExpr,
    ValueSpec,
    OpaqueTypeKind,
    OpaqueTypeSpec,
    ModuleSpec,
    Spec,
    ConcreteModuleType,
    QualifiedModuleType,
    ModuleType,
);

const _: () = {
    assert!(Node::BITS == 40);
};
