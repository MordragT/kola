use kola_collections::HashMap;
use kola_resolver::symbol::ModuleSym;
use kola_tree::meta::*;

use crate::types;

pub type TypeAnnotations = HashMap<ModuleSym, TypedNodes>;
pub type TypedNodes = MetaMap<TypePhase>;

#[derive(Clone, Copy, Debug)]
pub struct TypePhase;

impl Phase for TypePhase {
    type ModuleName = !;
    type TypeName = !;
    type ValueName = !;
    type AnyPat = types::MonoType;
    type LiteralPat = types::MonoType;
    type IdentPat = types::MonoType;
    type RecordFieldPat = types::Property;
    type RecordPat = types::MonoType;
    type VariantCasePat = types::Property;
    type VariantPat = types::MonoType;
    type PatError = !;
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
    type ExprError = !;
    type Expr = types::MonoType;
    type TypePath = types::PolyType;
    type TypeVar = !;
    type RecordFieldType = types::MonoType;
    type RecordType = types::MonoType;
    type VariantCaseType = types::MonoType;
    type VariantType = types::MonoType;
    type FuncType = types::MonoType;
    type TypeApplication = types::PolyType;
    type TypeExpr = types::PolyType;
    type TypeError = !;
    type Type = types::PolyType;
    type Vis = !;
    type ValueBind = types::PolyType;
    type TypeBind = types::PolyType;
    type OpaqueTypeBind = !;
    type ModuleBind = !;
    type ModuleTypeBind = !;
    type Bind = !;
    type Module = !;
    type ModulePath = !;
    type ModuleImport = !;
    type ModuleExpr = !;
    type ValueSpec = !;
    type OpaqueTypeKind = !;
    type OpaqueTypeSpec = !;
    type ModuleSpec = !;
    type Spec = !;
    type ModuleType = !;
}
