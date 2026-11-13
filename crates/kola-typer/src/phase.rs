use std::collections::HashMap;

use kola_resolver::symbol::ModuleSym;
use kola_tree::prelude::*;
use kola_types::types;

pub type TypeAnnotations = HashMap<ModuleSym, TypedNodes>;

kola_tree::define_side_table!(
    /// Type annotations produced by the type checker.
    TypedNodes {
        any_pats: Self::node::AnyPat => types::MonoType,
        literal_pats: Self::node::LiteralPat => types::MonoType,
        bind_pats: Self::node::BindPat => types::MonoType,
        list_el_pats: Self::node::ListElPat => types::MonoType,
        list_pats: Self::node::ListPat => types::MonoType,
        record_field_pats: Self::node::RecordFieldPat => types::LabeledType,
        record_pats: Self::node::RecordPat => types::MonoType,
        variant_tag_pats: Self::node::VariantTagPat => types::LabeledType,
        variant_pats: Self::node::VariantPat => types::MonoType,
        pats: Self::node::Pat => types::MonoType,
        literal_exprs: Self::node::LiteralExpr => types::MonoType,
        list_exprs: Self::node::ListExpr => types::MonoType,
        record_fields: Self::node::RecordField => types::LabeledType,
        record_exprs: Self::node::RecordExpr => types::MonoType,
        record_extend_exprs: Self::node::RecordExtendExpr => types::MonoType,
        record_restrict_exprs: Self::node::RecordRestrictExpr => types::MonoType,
        record_update_ops: Self::node::RecordUpdateOp => types::MonoType,
        record_update_exprs: Self::node::RecordUpdateExpr => types::MonoType,
        record_merge_exprs: Self::node::RecordMergeExpr => types::MonoType,
        qualified_exprs: Self::node::QualifiedExpr => types::MonoType,
        unary_ops: Self::node::UnaryOp => types::MonoType,
        unary_exprs: Self::node::UnaryExpr => types::MonoType,
        binary_ops: Self::node::BinaryOp => types::MonoType,
        binary_exprs: Self::node::BinaryExpr => types::MonoType,
        let_exprs: Self::node::LetExpr => types::MonoType,
        case_branches: Self::node::CaseBranch => types::MonoType,
        case_exprs: Self::node::CaseExpr => types::MonoType,
        if_exprs: Self::node::IfExpr => types::MonoType,
        lambda_exprs: Self::node::LambdaExpr => types::MonoType,
        call_exprs: Self::node::CallExpr => types::CompType,
        handler_clauses: Self::node::HandlerClause => types::LabeledType,
        handle_exprs: Self::node::HandleExpr => types::CompType,
        do_exprs: Self::node::DoExpr => types::CompType,
        tag_exprs: Self::node::TagExpr => types::MonoType,
        type_witness_exprs: Self::node::TypeWitnessExpr => types::MonoType,
        exprs: Self::node::Expr => types::MonoType,
        effect_op_types: Self::node::EffectOpType => types::LabeledType,
        effect_types: Self::node::EffectType => types::Row,
        qualified_types: Self::node::QualifiedType => types::PolyType,
        type_vars: Self::node::TypeVar => types::PolyType,
        label_or_vars: Self::node::LabelOrVar => types::LabelOrVar,
        record_field_types: Self::node::RecordFieldType => types::LabeledType,
        record_types: Self::node::RecordType => types::MonoType,
        tag_types: Self::node::TagType => types::LabeledType,
        variant_types: Self::node::VariantType => types::MonoType,
        func_types: Self::node::FuncType => types::MonoType,
        type_applications: Self::node::TypeApplication => types::PolyType,
        comp_types: Self::node::CompType => types::CompType,
        type_exprs: Self::node::TypeExpr => types::PolyType,
        type_var_binds: Self::node::TypeVarBind => types::TypeVar,
        forall_binders: Self::node::ForallBinder => Vec<types::TypeVar>,
        type_schemes: Self::node::TypeScheme => types::PolyType,
        value_binds: Self::node::ValueBind => types::PolyType,
        type_binds: Self::node::TypeBind => types::PolyType,
    }
);

impl TypedNodes {
    pub fn apply_mut(&mut self, s: &mut kola_types::subst::Substitution) {
        apply_map_mut(&mut self.any_pats, s);
        apply_map_mut(&mut self.literal_pats, s);
        apply_map_mut(&mut self.bind_pats, s);
        apply_map_mut(&mut self.list_el_pats, s);
        apply_map_mut(&mut self.list_pats, s);
        apply_map_mut(&mut self.record_field_pats, s);
        apply_map_mut(&mut self.record_pats, s);
        apply_map_mut(&mut self.variant_tag_pats, s);
        apply_map_mut(&mut self.variant_pats, s);
        apply_map_mut(&mut self.pats, s);
        apply_map_mut(&mut self.literal_exprs, s);
        apply_map_mut(&mut self.list_exprs, s);
        apply_map_mut(&mut self.record_fields, s);
        apply_map_mut(&mut self.record_exprs, s);
        apply_map_mut(&mut self.record_extend_exprs, s);
        apply_map_mut(&mut self.record_restrict_exprs, s);
        apply_map_mut(&mut self.record_update_ops, s);
        apply_map_mut(&mut self.record_update_exprs, s);
        apply_map_mut(&mut self.record_merge_exprs, s);
        apply_map_mut(&mut self.qualified_exprs, s);
        apply_map_mut(&mut self.unary_ops, s);
        apply_map_mut(&mut self.unary_exprs, s);
        apply_map_mut(&mut self.binary_ops, s);
        apply_map_mut(&mut self.binary_exprs, s);
        apply_map_mut(&mut self.let_exprs, s);
        apply_map_mut(&mut self.case_branches, s);
        apply_map_mut(&mut self.case_exprs, s);
        apply_map_mut(&mut self.if_exprs, s);
        apply_map_mut(&mut self.lambda_exprs, s);
        apply_map_mut(&mut self.call_exprs, s);
        apply_map_mut(&mut self.handler_clauses, s);
        apply_map_mut(&mut self.handle_exprs, s);
        apply_map_mut(&mut self.do_exprs, s);
        apply_map_mut(&mut self.tag_exprs, s);
        apply_map_mut(&mut self.type_witness_exprs, s);
        apply_map_mut(&mut self.exprs, s);
        apply_map_mut(&mut self.effect_op_types, s);
        apply_map_mut(&mut self.effect_types, s);
        apply_map_mut(&mut self.qualified_types, s);
        apply_map_mut(&mut self.type_vars, s);
        apply_map_mut(&mut self.label_or_vars, s);
        apply_map_mut(&mut self.record_field_types, s);
        apply_map_mut(&mut self.record_types, s);
        apply_map_mut(&mut self.tag_types, s);
        apply_map_mut(&mut self.variant_types, s);
        apply_map_mut(&mut self.func_types, s);
        apply_map_mut(&mut self.type_applications, s);
        apply_map_mut(&mut self.comp_types, s);
        apply_map_mut(&mut self.type_exprs, s);
        apply_map_mut(&mut self.type_var_binds, s);
        apply_map_mut(&mut self.forall_binders, s);
        apply_map_mut(&mut self.type_schemes, s);
        apply_map_mut(&mut self.value_binds, s);
        apply_map_mut(&mut self.type_binds, s);
    }
}

fn apply_map_mut<T, N>(
    map: &mut kola_tree::prelude::SideMap<N, T>,
    s: &mut kola_types::subst::Substitution,
)
where
    T: kola_subst::Substitutable<kola_types::subst::Substitution> + 'static,
    N: 'static,
{
    for (_, value) in map.iter_mut() {
        value.apply_mut(s);
    }
}
