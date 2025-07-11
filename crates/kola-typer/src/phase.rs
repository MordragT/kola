use kola_collections::HashMap;
use kola_resolver::symbol::ModuleSym;
use kola_tree::meta::*;

use crate::types;

pub type TypeAnnotations = HashMap<ModuleSym, TypedNodes>;
pub type TypedNodes = MetaMap<TypePhase>;

#[derive(Clone, Copy, Debug)]
pub struct TypePhase;

impl Phase for TypePhase {
    type EffectName = !;
    type FunctorName = !;
    type ModuleTypeName = !;
    type ModuleName = !;
    type TypeName = !;
    type ValueName = !;

    type AnyPat = types::MonoType;
    type LiteralPat = types::MonoType;
    type BindPat = types::MonoType;
    type ListElPat = types::MonoType;
    type ListPat = types::MonoType;
    type RecordFieldPat = types::LabeledType;
    type RecordPat = types::MonoType;
    type VariantTagPat = types::LabeledType;
    type VariantPat = types::MonoType;
    type PatError = !;
    type Pat = types::MonoType;

    type LiteralExpr = types::MonoType;
    type ListExpr = types::MonoType;
    type RecordField = types::LabeledType;
    type RecordExpr = types::MonoType;
    type RecordExtendExpr = types::MonoType;
    type RecordRestrictExpr = types::MonoType;
    type RecordUpdateOp = types::MonoType;
    type RecordUpdateExpr = types::MonoType;
    type RecordMergeExpr = types::MonoType;
    type FieldPath = !;
    type QualifiedExpr = types::MonoType;
    type TagExpr = types::MonoType;
    type UnaryOp = types::MonoType;
    type UnaryExpr = types::MonoType;
    type BinaryOp = types::MonoType;
    type BinaryExpr = types::MonoType;
    type LetExpr = types::MonoType;
    type CaseBranch = types::MonoType;
    type CaseExpr = types::MonoType;
    type IfExpr = types::MonoType;
    type LambdaExpr = types::MonoType;
    type CallExpr = types::CompType;
    type HandlerClause = types::MonoType;
    type HandleExpr = types::CompType;
    type DoExpr = types::CompType;
    type TypeRepExpr = types::MonoType;
    type ExprError = !;
    // TODO this is MonoType for now but this should change when CompTypes are properly handled
    type Expr = types::MonoType;

    // Effects are implemented via row types
    type QualifiedEffectType = types::RowType;
    type EffectOpType = types::LabeledType;
    type EffectRowType = types::RowType;
    type EffectType = types::RowType;

    type QualifiedType = types::PolyType;
    type TypeVar = types::PolyType;
    type RecordFieldType = types::LabeledType;
    type RecordType = types::MonoType;
    type TagType = types::LabeledType;
    type VariantType = types::MonoType;
    type FuncType = types::MonoType;
    type TypeApplication = types::PolyType;
    type CompType = types::CompType;
    type Type = types::PolyType;
    type TypeError = !;
    type Kind = types::Kind;
    type TypeVarBind = types::KindedVar;
    type WithBinder = Vec<types::KindedVar>;
    type ForallBinder = Vec<types::KindedVar>;
    type TypeScheme = types::PolyType;

    type Vis = !;
    type ValueBind = types::PolyType; // is a MonoType for the typer but a PolyType for the Printer (after generalization)
    type TypeBind = types::PolyType;
    type OpaqueTypeBind = !;
    type EffectTypeBind = types::RowType;
    type ModuleBind = !;
    type ModuleTypeBind = !;
    type FunctorBind = !;
    type Bind = !;

    type ModuleError = !;
    type Module = !;
    type ModulePath = !;
    type ModuleImport = !;
    type FunctorApp = !;
    type ModuleExpr = !;

    type ValueSpec = !;
    type OpaqueTypeKind = !;
    type OpaqueTypeSpec = !;
    type ModuleSpec = !;
    type Spec = !;
    type ConcreteModuleType = !;
    type QualifiedModuleType = !;
    type ModuleType = !;
}
