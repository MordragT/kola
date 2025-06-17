use paste::paste;

use crate::{
    id::Id,
    node::{self, NodeKind},
    tree::TreeView,
};

macro_rules! define_folder_trait {
    ($($variant:ident),* $(,)?) =>  {
        paste!{
        pub trait Folder<T: TreeView> {
            fn fold(&mut self, tree: &mut T) {
                let mut stack = (0..tree.count()).collect::<Vec<_>>();

                while let Some(id) = stack.pop() {
                    match tree.get(id).kind() {
                        $(NodeKind::$variant => self.[<fold_ $variant:snake:lower>](Id::unchecked_from_usize(id), tree),)*
                    }
                }
            }

            $(
                #[inline]
                fn [<fold_ $variant:snake:lower>](&mut self, _id: Id<node::$variant>, _tree: &mut T) {}
            )*
        }}
    };
}

define_folder_trait!(
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
    ListExpr,
    RecordField,
    RecordExpr,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
    SelectExpr,
    PathExpr,
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
