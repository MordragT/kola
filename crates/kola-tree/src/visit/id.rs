use derive_more::From;

use crate::{id::NodeId, node};

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BranchId {
    // Patterns
    RecordFieldPat(NodeId<node::RecordFieldPat>),
    RecordPat(NodeId<node::RecordPat>),
    VariantCasePat(NodeId<node::VariantCasePat>),
    VariantPat(NodeId<node::VariantPat>),
    Pat(NodeId<node::Pat>),
    // Expr
    Path(NodeId<node::PathExpr>),
    List(NodeId<node::ListExpr>),
    RecordField(NodeId<node::RecordField>),
    Record(NodeId<node::RecordExpr>),
    RecordFieldPath(NodeId<node::RecordFieldPath>),
    RecordExtend(NodeId<node::RecordExtendExpr>),
    RecordRestrict(NodeId<node::RecordRestrictExpr>),
    RecordUpdate(NodeId<node::RecordUpdateExpr>),
    Unary(NodeId<node::UnaryExpr>),
    Binary(NodeId<node::BinaryExpr>),
    Let(NodeId<node::LetExpr>),
    CaseBranch(NodeId<node::CaseBranch>),
    Case(NodeId<node::CaseExpr>),
    If(NodeId<node::IfExpr>),
    Lambda(NodeId<node::LambdaExpr>),
    Call(NodeId<node::CallExpr>),
    Expr(NodeId<node::Expr>),
    // Types
    TypePath(NodeId<node::TypePath>),
    RecordFieldType(NodeId<node::RecordFieldType>),
    RecordType(NodeId<node::RecordType>),
    VariantCaseType(NodeId<node::VariantCaseType>),
    VariantType(NodeId<node::VariantType>),
    FuncType(NodeId<node::FuncType>),
    TypeApplication(NodeId<node::TypeApplication>),
    TypeExpr(NodeId<node::TypeExpr>),
    Type(NodeId<node::Type>),
    // Modules
    ValueBind(NodeId<node::ValueBind>),
    TypeBind(NodeId<node::TypeBind>),
    OpaqueTypeBind(NodeId<node::OpaqueTypeBind>),
    ModuleBind(NodeId<node::ModuleBind>),
    ModuleTypeBind(NodeId<node::ModuleTypeBind>),
    Bind(NodeId<node::Bind>),
    Module(NodeId<node::Module>),
    ValueSpec(NodeId<node::ValueSpec>),
    OpaqueTypeSpec(NodeId<node::OpaqueTypeSpec>),
    ModuleSpec(NodeId<node::ModuleSpec>),
    Spec(NodeId<node::Spec>),
    ModuleType(NodeId<node::ModuleType>),
}

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LeafId {
    Name(NodeId<node::Name>),
    // Patterns
    AnyPat(NodeId<node::AnyPat>),
    LiteralPat(NodeId<node::LiteralPat>),
    IdentPat(NodeId<node::IdentPat>),
    PatError(NodeId<node::PatError>),
    // Expr
    Literal(NodeId<node::LiteralExpr>),
    UnaryOp(NodeId<node::UnaryOp>),
    BinaryOp(NodeId<node::BinaryOp>),
    RecordUpdateOp(NodeId<node::RecordUpdateOp>),
    ExprError(NodeId<node::ExprError>),
    // Types
    TypeVar(NodeId<node::TypeVar>),
    TypeError(NodeId<node::TypeError>),
    // Modules
    OpaqueTypeKind(NodeId<node::OpaqueTypeKind>),
}
