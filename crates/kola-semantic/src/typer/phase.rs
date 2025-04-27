use kola_tree::meta::*;

use crate::types;

pub type TypeInfo = Metadata<TypePhase>;

#[derive(Clone, Copy, Debug)]
pub struct TypePhase;

impl Phase for TypePhase {
    type Name = Empty;
    type AnyPat = types::MonoType;
    type LiteralPat = types::MonoType;
    type IdentPat = types::MonoType;
    type RecordFieldPat = types::Property;
    type RecordPat = types::MonoType;
    type VariantCasePat = types::Property;
    type VariantPat = types::MonoType;
    type PatError = Empty;
    type Pat = types::MonoType;
    type LiteralExpr = types::MonoType;
    type PathExpr = types::MonoType;
    type ListExpr = types::MonoType;
    type RecordField = types::Property;
    type RecordExpr = types::MonoType;
    type RecordExtendExpr = types::MonoType;
    type RecordRestrictExpr = types::MonoType;
    type RecordUpdateOp = types::MonoType;
    type RecordUpdateExpr = types::MonoType;
    type UnaryOp = types::MonoType;
    type UnaryExpr = types::MonoType;
    type BinaryOp = types::MonoType;
    type BinaryExpr = types::MonoType;
    type LetExpr = types::MonoType;
    type CaseBranch = types::MonoType;
    type CaseExpr = types::MonoType;
    type IfExpr = types::MonoType;
    type LambdaExpr = types::MonoType;
    type CallExpr = types::MonoType;
    type ExprError = Empty;
    type Expr = types::MonoType;
    type TypePath = Stub<types::PolyType>;
    type TypeVar = Empty;
    type RecordFieldType = types::MonoType;
    type RecordType = types::MonoType;
    type VariantCaseType = types::MonoType;
    type VariantType = types::MonoType;
    type FuncType = types::MonoType;
    type TypeApplication = Stub<types::PolyType>;
    type TypeExpr = Stub<types::PolyType>;
    type TypeError = Empty;
    type Type = Stub<types::PolyType>;
    type Vis = Empty;
    type ValueBind = types::MonoType;
    type TypeBind = Stub<types::PolyType>;
    type OpaqueTypeBind = Empty;
    type ModuleBind = Empty;
    type ModuleTypeBind = Empty;
    type Bind = Empty;
    type Module = Empty;
    type ModulePath = Empty;
    type ModuleImport = Empty;
    type ModuleExpr = Empty;
    type ValueSpec = Empty;
    type OpaqueTypeKind = Empty;
    type OpaqueTypeSpec = Empty;
    type ModuleSpec = Empty;
    type Spec = Empty;
    type ModuleType = Empty;
}
