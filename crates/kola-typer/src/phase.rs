use std::collections::HashMap;

use kola_resolver::symbol::ModuleSym;
use kola_subst::Substitutable;
use kola_tree::prelude::*;
use kola_types::types;

pub type TypeAnnotations = HashMap<ModuleSym, TypedNodes>;

/// Type annotations produced by the type checker.
#[derive(Debug, Clone, Default, RelationStorage, Merge, Substitutable)]
pub struct TypedNodes {
    #[relation]
    pub any_pats: MaybeVec<node::AnyPat, types::MonoType>,
    #[relation]
    pub literal_pats: MaybeVec<node::LiteralPat, types::MonoType>,
    #[relation]
    pub bind_pats: MaybeVec<node::BindPat, types::MonoType>,
    #[relation]
    pub list_el_pats: MaybeVec<node::ListElPat, types::MonoType>,
    #[relation]
    pub list_pats: MaybeVec<node::ListPat, types::MonoType>,
    #[relation]
    pub record_field_pats: MaybeVec<node::RecordFieldPat, types::LabeledType>,
    #[relation]
    pub record_pats: MaybeVec<node::RecordPat, types::MonoType>,
    #[relation]
    pub variant_tag_pats: MaybeVec<node::VariantTagPat, types::LabeledType>,
    #[relation]
    pub variant_pats: MaybeVec<node::VariantPat, types::MonoType>,
    #[relation]
    pub pats: MaybeVec<node::Pat, types::MonoType>,
    #[relation]
    pub literal_exprs: MaybeVec<node::LiteralExpr, types::MonoType>,
    #[relation]
    pub list_exprs: MaybeVec<node::ListExpr, types::MonoType>,
    #[relation]
    pub record_fields: MaybeVec<node::RecordField, types::LabeledType>,
    #[relation]
    pub record_exprs: MaybeVec<node::RecordExpr, types::MonoType>,
    #[relation]
    pub record_extend_exprs: MaybeVec<node::RecordExtendExpr, types::MonoType>,
    #[relation]
    pub record_restrict_exprs: MaybeVec<node::RecordRestrictExpr, types::MonoType>,
    #[relation]
    pub record_update_ops: MaybeVec<node::RecordUpdateOp, types::MonoType>,
    #[relation]
    pub record_update_exprs: MaybeVec<node::RecordUpdateExpr, types::MonoType>,
    #[relation]
    pub record_merge_exprs: MaybeVec<node::RecordMergeExpr, types::MonoType>,
    #[relation]
    pub qualified_exprs: MaybeVec<node::QualifiedExpr, types::MonoType>,
    #[relation]
    pub unary_ops: MaybeVec<node::UnaryOp, types::MonoType>,
    #[relation]
    pub unary_exprs: MaybeVec<node::UnaryExpr, types::MonoType>,
    #[relation]
    pub binary_ops: MaybeVec<node::BinaryOp, types::MonoType>,
    #[relation]
    pub binary_exprs: MaybeVec<node::BinaryExpr, types::MonoType>,
    #[relation]
    pub let_exprs: MaybeVec<node::LetExpr, types::MonoType>,
    #[relation]
    pub case_branches: MaybeVec<node::CaseBranch, types::MonoType>,
    #[relation]
    pub case_exprs: MaybeVec<node::CaseExpr, types::MonoType>,
    #[relation]
    pub if_exprs: MaybeVec<node::IfExpr, types::MonoType>,
    #[relation]
    pub lambda_exprs: MaybeVec<node::LambdaExpr, types::MonoType>,
    #[relation]
    pub call_exprs: MaybeVec<node::CallExpr, types::CompType>,
    #[relation]
    pub handler_clauses: MaybeVec<node::HandlerClause, types::LabeledType>,
    #[relation]
    pub handle_exprs: MaybeVec<node::HandleExpr, types::CompType>,
    #[relation]
    pub do_exprs: MaybeVec<node::DoExpr, types::CompType>,
    #[relation]
    pub tag_exprs: MaybeVec<node::TagExpr, types::MonoType>,
    #[relation]
    pub type_witness_exprs: MaybeVec<node::TypeWitnessExpr, types::MonoType>,
    #[relation]
    pub exprs: MaybeVec<node::Expr, types::MonoType>,
    #[relation]
    pub effect_op_types: MaybeVec<node::EffectOpType, types::LabeledType>,
    #[relation]
    pub effect_types: MaybeVec<node::EffectType, types::Row>,
    #[relation]
    pub qualified_types: MaybeVec<node::QualifiedType, types::PolyType>,
    #[relation]
    pub type_vars: MaybeVec<node::TypeVar, types::PolyType>,
    #[relation]
    pub label_or_vars: MaybeVec<node::LabelOrVar, types::LabelOrVar>,
    #[relation]
    pub record_field_types: MaybeVec<node::RecordFieldType, types::LabeledType>,
    #[relation]
    pub record_types: MaybeVec<node::RecordType, types::MonoType>,
    #[relation]
    pub tag_types: MaybeVec<node::TagType, types::LabeledType>,
    #[relation]
    pub variant_types: MaybeVec<node::VariantType, types::MonoType>,
    #[relation]
    pub func_types: MaybeVec<node::FuncType, types::MonoType>,
    #[relation]
    pub type_applications: MaybeVec<node::TypeApplication, types::PolyType>,
    #[relation]
    pub comp_types: MaybeVec<node::CompType, types::CompType>,
    #[relation]
    pub type_exprs: MaybeVec<node::TypeExpr, types::PolyType>,
    #[relation]
    pub type_var_binds: MaybeVec<node::TypeVarBind, types::TypeVar>,
    #[relation]
    pub forall_binders: MaybeVec<node::ForallBinder, Vec<types::TypeVar>>,
    #[relation]
    pub type_schemes: MaybeVec<node::TypeScheme, types::PolyType>,
    #[relation]
    pub value_binds: MaybeVec<node::ValueBind, types::PolyType>,
    #[relation]
    pub type_binds: MaybeVec<node::TypeBind, types::PolyType>,
}

impl TypedNodes {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn get_opt<T: 'static, V: 'static>(&self, id: Id<T>) -> Option<&V>
    where
        Self: PartialRelation<T, V>,
    {
        PartialRelation::<T, V>::get_opt(self, id)
    }

    #[inline]
    pub fn set<T: 'static, V: 'static>(&mut self, id: Id<T>, value: V)
    where
        Self: PartialRelationSet<T, V>,
    {
        PartialRelationSet::<T, V>::set(self, id, value)
    }

    #[inline]
    #[track_caller]
    pub fn get_unchecked<T: 'static, V: 'static>(&self, id: Id<T>) -> &V
    where
        Self: PartialRelation<T, V>,
    {
        self.get_opt(id).expect("typed metadata missing")
    }

    #[inline]
    #[track_caller]
    pub fn get_unchecked_mut<T: 'static, V: 'static>(&mut self, id: Id<T>) -> &mut V
    where
        Self: PartialRelationMut<T, V>,
    {
        PartialRelationMut::<T, V>::get_mut(self, id).expect("typed metadata missing")
    }
}
