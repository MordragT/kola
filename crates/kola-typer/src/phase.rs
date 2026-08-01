use std::collections::HashMap;

use pastey::paste;

use kola_resolver::symbol::ModuleSym;
use kola_subst::Substitutable;
use kola_tree::prelude::*;
use kola_types::{subst::Substitution, types};

pub type TypeAnnotations = HashMap<ModuleSym, TypedNodes>;

macro_rules! define_typed_nodes {
    (
        $(
            $field:ident : MetaMap<$node:ty, $value:ty>
        ),* $(,)?
    ) => {
        #[derive(Debug, Clone)]
        pub struct TypedNodes {
            $(
                pub $field: MetaMap<$node, $value>,
            )*
        }

        $(
          impl GetOpt<$node> for TypedNodes {
              type Item = $value;

              fn get_opt(&self, id: Id<$node>) -> Option<&Self::Item> {
                  self.$field.get_opt(id)
              }

              fn get_opt_mut(&mut self, id: Id<$node>) -> Option<&mut Self::Item> {
                  self.$field.get_opt_mut(id)
              }

              fn set(&mut self, id: Id<$node>, value: Self::Item) -> Option<Self::Item> {
                  self.$field.set(id, value)
              }
          }

          impl Col<$node> for TypedNodes {
              type Column = MetaMap<$node, $value>;
              type Ids<'a> = <MetaMap::<$node, $value> as Col<$node>>::Ids<'a>;


              #[inline]
              fn col(&self) -> &Self::Column {
                  &self.$field
              }

              #[inline]
              fn col_mut(&mut self) -> &mut Self::Column {
                  &mut self.$field
              }

              #[inline]
              fn ids<'a>(&'a self) -> Self::Ids<'a> {
                  self.$field.ids()
              }
          }
        )*

        impl Substitutable<Substitution> for TypedNodes {
            fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
                let mut changed = false;

                $(
                    let $field = match self.$field.try_apply(s) {
                        Some(x) => {
                            changed = true;
                            x
                        }
                        None => self.$field.clone(),
                    };
                )*

                changed.then_some(Self {
                    $(
                        $field,
                    )*
                })
            }
        }

        paste!{
            impl Default for TypedNodes {
                fn default() -> Self {
                    Self {
                        $(
                            $field: MetaMap::new(),
                        )*
                    }
                }
            }
        }

        impl TypedNodes {
            pub fn new() -> Self {
                Self::default()
            }

            pub fn extend(&mut self, other: Self) {
                $(
                    self.$field.extend(other.$field.into_iter());
                )*
            }
        }
    };
}

define_typed_nodes! {
    any_pats: MetaMap<node::AnyPat, types::MonoType>,
    literal_pats: MetaMap<node::LiteralPat, types::MonoType>,
    bind_pats: MetaMap<node::BindPat, types::MonoType>,
    list_el_pats: MetaMap<node::ListElPat, types::MonoType>,
    list_pats: MetaMap<node::ListPat, types::MonoType>,
    record_field_pats: MetaMap<node::RecordFieldPat, types::LabeledType>,
    record_pats: MetaMap<node::RecordPat, types::MonoType>,
    variant_tag_pats: MetaMap<node::VariantTagPat, types::LabeledType>,
    variant_pats: MetaMap<node::VariantPat, types::MonoType>,
    pats: MetaMap<node::Pat, types::MonoType>,

    literal_exprs: MetaMap<node::LiteralExpr, types::MonoType>,
    list_exprs: MetaMap<node::ListExpr, types::MonoType>,
    record_fields: MetaMap<node::RecordField, types::LabeledType>,
    record_exprs: MetaMap<node::RecordExpr, types::MonoType>,
    record_extend_exprs: MetaMap<node::RecordExtendExpr, types::MonoType>,
    record_restrict_exprs: MetaMap<node::RecordRestrictExpr, types::MonoType>,
    record_update_ops: MetaMap<node::RecordUpdateOp, types::MonoType>,
    record_update_exprs: MetaMap<node::RecordUpdateExpr, types::MonoType>,
    record_merge_exprs: MetaMap<node::RecordMergeExpr, types::MonoType>,
    qualified_exprs: MetaMap<node::QualifiedExpr, types::MonoType>,
    unary_ops: MetaMap<node::UnaryOp, types::MonoType>,
    unary_exprs: MetaMap<node::UnaryExpr, types::MonoType>,
    binary_ops: MetaMap<node::BinaryOp, types::MonoType>,
    binary_exprs: MetaMap<node::BinaryExpr, types::MonoType>,
    let_exprs: MetaMap<node::LetExpr, types::MonoType>,
    case_branches: MetaMap<node::CaseBranch, types::MonoType>,
    case_exprs: MetaMap<node::CaseExpr, types::MonoType>,
    if_exprs: MetaMap<node::IfExpr, types::MonoType>,
    lambda_exprs: MetaMap<node::LambdaExpr, types::MonoType>,
    call_exprs: MetaMap<node::CallExpr, types::CompType>,
    handler_clauses: MetaMap<node::HandlerClause, types::LabeledType>,
    handle_exprs: MetaMap<node::HandleExpr, types::CompType>,
    do_exprs: MetaMap<node::DoExpr, types::CompType>,
    tag_exprs: MetaMap<node::TagExpr, types::MonoType>,
    type_witness_exprs: MetaMap<node::TypeWitnessExpr, types::MonoType>,
    exprs: MetaMap<node::Expr, types::MonoType>,

    effect_op_types: MetaMap<node::EffectOpType, types::LabeledType>,
    effect_types: MetaMap<node::EffectType, types::Row>,

    qualified_types: MetaMap<node::QualifiedType, types::PolyType>,
    type_vars: MetaMap<node::TypeVar, types::PolyType>,
    label_or_vars: MetaMap<node::LabelOrVar, types::LabelOrVar>,
    record_field_types: MetaMap<node::RecordFieldType, types::LabeledType>,
    record_types: MetaMap<node::RecordType, types::MonoType>,
    tag_types: MetaMap<node::TagType, types::LabeledType>,
    variant_types: MetaMap<node::VariantType, types::MonoType>,
    func_types: MetaMap<node::FuncType, types::MonoType>,
    type_applications: MetaMap<node::TypeApplication, types::PolyType>,
    comp_types: MetaMap<node::CompType, types::CompType>,
    type_exprs: MetaMap<node::TypeExpr, types::PolyType>,
    type_var_binds: MetaMap<node::TypeVarBind, types::TypeVar>,
    forall_binders: MetaMap<node::ForallBinder, Vec<types::TypeVar>>,
    type_schemes: MetaMap<node::TypeScheme, types::PolyType>,

    value_binds: MetaMap<node::ValueBind, types::PolyType>,
    type_binds: MetaMap<node::TypeBind, types::PolyType>,
}
