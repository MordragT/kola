use kola_collections::HashMap;
use kola_resolver::symbol::ModuleSym;
use kola_tree::meta::*;
use kola_types::types;

pub type TypeAnnotations = HashMap<ModuleSym, TypedNodes>;
pub type TypedNodes = MetaMap<TypePhase>;

#[derive(Clone, Copy, Debug)]
pub struct TypePhase;

impl Phase for TypePhase {
    type FunctorName = !;
    type ModuleTypeName = !;
    type ModuleName = !;
    type KindName = !;
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
    type HandlerClause = types::LabeledType;
    type HandleExpr = types::CompType;
    type DoExpr = types::CompType;
    type TagExpr = types::MonoType;
    type TypeWitnessExpr = types::MonoType;
    type ExprError = !;
    type Expr = types::MonoType;

    // Effects are implemented via row types
    type EffectOpType = types::LabeledType;
    type EffectType = types::Row;

    type QualifiedType = types::PolyType;
    type TypeVar = types::PolyType;
    type LabelOrVar = types::LabelOrVar;
    type RecordFieldType = types::LabeledType;
    type RecordType = types::MonoType;
    type TagType = types::LabeledType;
    type VariantType = types::MonoType;
    type FuncType = types::MonoType;
    type TypeApplication = types::PolyType;
    type CompType = types::CompType;
    type Type = types::PolyType;
    type TypeError = !;
    type TypeVarBind = types::TypeVar;
    type ForallBinder = Vec<types::TypeVar>;
    type TypeScheme = types::PolyType;

    type BindError = !;
    type Vis = !;
    type ValueBind = types::PolyType; // is a MonoType for the typer but a PolyType for the Printer (after generalization)
    type TypeBind = types::PolyType;
    type OpaqueTypeBind = !;
    type ModuleBind = !;
    type ModuleTypeBind = !;
    type FunctorParam = !;
    type FunctorBind = !;
    type Bind = !;

    type ModuleError = !;
    type Module = !;
    type ModulePath = !;
    type ModuleImport = !;
    type FunctorArgs = !;
    type FunctorApp = !;
    type ModuleExpr = !;

    type SpecError = !;
    type ValueSpec = !;
    type OpaqueTypeSpec = !;
    type ModuleSpec = !;
    type Spec = !;
    type ConcreteModuleType = !;
    type QualifiedModuleType = !;
    type ModuleType = !;
}
