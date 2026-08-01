use std::collections::HashMap;

use kola_resolver::symbol::ModuleSym;
use kola_tree::prelude::*;
use kola_types::types;

pub type TypeAnnotations = HashMap<ModuleSym, TypedNodes>;

kola_tree::define_side_table!(TypedNodes {
    any_pats: SideMap<node::AnyPat, types::MonoType>,
    literal_pats: SideMap<node::LiteralPat, types::MonoType>,
    bind_pats: SideMap<node::BindPat, types::MonoType>,
    list_el_pats: SideMap<node::ListElPat, types::MonoType>,
    list_pats: SideMap<node::ListPat, types::MonoType>,
    record_field_pats: SideMap<node::RecordFieldPat, types::LabeledType>,
    record_pats: SideMap<node::RecordPat, types::MonoType>,
    variant_tag_pats: SideMap<node::VariantTagPat, types::LabeledType>,
    variant_pats: SideMap<node::VariantPat, types::MonoType>,
    pats: SideMap<node::Pat, types::MonoType>,

    literal_exprs: SideMap<node::LiteralExpr, types::MonoType>,
    list_exprs: SideMap<node::ListExpr, types::MonoType>,
    record_fields: SideMap<node::RecordField, types::LabeledType>,
    record_exprs: SideMap<node::RecordExpr, types::MonoType>,
    record_extend_exprs: SideMap<node::RecordExtendExpr, types::MonoType>,
    record_restrict_exprs: SideMap<node::RecordRestrictExpr, types::MonoType>,
    record_update_ops: SideMap<node::RecordUpdateOp, types::MonoType>,
    record_update_exprs: SideMap<node::RecordUpdateExpr, types::MonoType>,
    record_merge_exprs: SideMap<node::RecordMergeExpr, types::MonoType>,
    qualified_exprs: SideMap<node::QualifiedExpr, types::MonoType>,
    unary_ops: SideMap<node::UnaryOp, types::MonoType>,
    unary_exprs: SideMap<node::UnaryExpr, types::MonoType>,
    binary_ops: SideMap<node::BinaryOp, types::MonoType>,
    binary_exprs: SideMap<node::BinaryExpr, types::MonoType>,
    let_exprs: SideMap<node::LetExpr, types::MonoType>,
    case_branches: SideMap<node::CaseBranch, types::MonoType>,
    case_exprs: SideMap<node::CaseExpr, types::MonoType>,
    if_exprs: SideMap<node::IfExpr, types::MonoType>,
    lambda_exprs: SideMap<node::LambdaExpr, types::MonoType>,
    call_exprs: SideMap<node::CallExpr, types::CompType>,
    handler_clauses: SideMap<node::HandlerClause, types::LabeledType>,
    handle_exprs: SideMap<node::HandleExpr, types::CompType>,
    do_exprs: SideMap<node::DoExpr, types::CompType>,
    tag_exprs: SideMap<node::TagExpr, types::MonoType>,
    type_witness_exprs: SideMap<node::TypeWitnessExpr, types::MonoType>,
    exprs: SideMap<node::Expr, types::MonoType>,

    effect_op_types: SideMap<node::EffectOpType, types::LabeledType>,
    effect_types: SideMap<node::EffectType, types::Row>,

    qualified_types: SideMap<node::QualifiedType, types::PolyType>,
    type_vars: SideMap<node::TypeVar, types::PolyType>,
    label_or_vars: SideMap<node::LabelOrVar, types::LabelOrVar>,
    record_field_types: SideMap<node::RecordFieldType, types::LabeledType>,
    record_types: SideMap<node::RecordType, types::MonoType>,
    tag_types: SideMap<node::TagType, types::LabeledType>,
    variant_types: SideMap<node::VariantType, types::MonoType>,
    func_types: SideMap<node::FuncType, types::MonoType>,
    type_applications: SideMap<node::TypeApplication, types::PolyType>,
    comp_types: SideMap<node::CompType, types::CompType>,
    type_exprs: SideMap<node::TypeExpr, types::PolyType>,
    type_var_binds: SideMap<node::TypeVarBind, types::TypeVar>,
    forall_binders: SideMap<node::ForallBinder, Vec<types::TypeVar>>,
    type_schemes: SideMap<node::TypeScheme, types::PolyType>,

    value_binds: SideMap<node::ValueBind, types::PolyType>,
    type_binds: SideMap<node::TypeBind, types::PolyType>,
});
