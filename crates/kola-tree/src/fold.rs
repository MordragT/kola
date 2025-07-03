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
    TagExpr,
    ExprError,
    Expr,
    // Types
    QualifiedType,
    TypeVar,
    RecordFieldType,
    RecordType,
    VariantTagType,
    VariantType,
    FuncType,
    TypeApplication,
    Type,
    TypeError,
    TypeScheme,
    // Modules
    Vis,
    ValueBind,
    TypeBind,
    OpaqueTypeBind,
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
    ModuleType
);
