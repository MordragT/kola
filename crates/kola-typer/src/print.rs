use kola_print::prelude::*;
use kola_tree::{node::AnyId, print::Decorator, query::GetOpt};
use owo_colors::OwoColorize;

use crate::phase::TypedNodes;

#[derive(Debug, Clone)]
pub struct TypeDecorator<'a>(pub &'a TypedNodes);

impl<'a> Decorator<'a> for TypeDecorator<'a> {
    fn decorate(&self, notation: Notation<'a>, with: AnyId, arena: &'a Bump) -> Notation<'a> {
        let ty = match with {
            // Patterns
            AnyId::AnyPat(id) => self.0.any_pats.get_unchecked(id).green().display_in(arena),
            AnyId::LiteralPat(id) => self
                .0
                .literal_pats
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::BindPat(id) => self.0.bind_pats.get_unchecked(id).green().display_in(arena),
            AnyId::ListElPat(id) => self
                .0
                .list_el_pats
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::ListPat(id) => self.0.list_pats.get_unchecked(id).green().display_in(arena),
            AnyId::RecordFieldPat(id) => self
                .0
                .record_field_pats
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::RecordPat(id) => self
                .0
                .record_pats
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::VariantTagPat(id) => self
                .0
                .variant_tag_pats
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::VariantPat(id) => self
                .0
                .variant_pats
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::Pat(id) => self.0.pats.get_unchecked(id).green().display_in(arena),

            // Expressions
            AnyId::LiteralExpr(id) => self
                .0
                .literal_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::ListExpr(id) => self
                .0
                .list_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::RecordField(id) => self
                .0
                .record_fields
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::RecordExpr(id) => self
                .0
                .record_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::RecordExtendExpr(id) => self
                .0
                .record_extend_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::RecordRestrictExpr(id) => self
                .0
                .record_restrict_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::RecordUpdateOp(id) => self
                .0
                .record_update_ops
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::RecordUpdateExpr(id) => self
                .0
                .record_update_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::RecordMergeExpr(id) => self
                .0
                .record_merge_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::QualifiedExpr(id) => self
                .0
                .qualified_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::UnaryOp(id) => self.0.unary_ops.get_unchecked(id).green().display_in(arena),
            AnyId::UnaryExpr(id) => self
                .0
                .unary_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::BinaryOp(id) => self
                .0
                .binary_ops
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::BinaryExpr(id) => self
                .0
                .binary_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::LetExpr(id) => self.0.let_exprs.get_unchecked(id).green().display_in(arena),
            AnyId::CaseBranch(id) => self
                .0
                .case_branches
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::CaseExpr(id) => self
                .0
                .case_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::IfExpr(id) => self.0.if_exprs.get_unchecked(id).green().display_in(arena),
            AnyId::LambdaExpr(id) => self
                .0
                .lambda_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::CallExpr(id) => self
                .0
                .call_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::HandlerClause(id) => self
                .0
                .handler_clauses
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::HandleExpr(id) => self
                .0
                .handle_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::DoExpr(id) => self.0.do_exprs.get_unchecked(id).green().display_in(arena),
            AnyId::TagExpr(id) => self.0.tag_exprs.get_unchecked(id).green().display_in(arena),
            AnyId::TypeWitnessExpr(id) => self
                .0
                .type_witness_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::Expr(id) => self.0.exprs.get_unchecked(id).green().display_in(arena),

            // Effects
            AnyId::EffectOpType(id) => self
                .0
                .effect_op_types
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::EffectType(id) => self
                .0
                .effect_types
                .get_unchecked(id)
                .green()
                .display_in(arena),

            // Types
            AnyId::QualifiedType(id) => self
                .0
                .qualified_types
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::TypeVar(id) => self.0.type_vars.get_unchecked(id).green().display_in(arena),
            AnyId::LabelOrVar(id) => self
                .0
                .label_or_vars
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::RecordFieldType(id) => self
                .0
                .record_field_types
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::RecordType(id) => self
                .0
                .record_types
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::TagType(id) => self.0.tag_types.get_unchecked(id).green().display_in(arena),
            AnyId::VariantType(id) => self
                .0
                .variant_types
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::FuncType(id) => self
                .0
                .func_types
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::TypeApplication(id) => self
                .0
                .type_applications
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::CompType(id) => self
                .0
                .comp_types
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::TypeExpr(id) => self
                .0
                .type_exprs
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::TypeVarBind(id) => self
                .0
                .type_var_binds
                .get_unchecked(id)
                .green()
                .display_in(arena),
            // AnyId::ForallBinder(id) => self
            //     .0
            //     .forall_binders
            //     .get_unchecked(id)
            //     .green()
            //     .display_in(arena),
            AnyId::TypeScheme(id) => self
                .0
                .type_schemes
                .get_unchecked(id)
                .green()
                .display_in(arena),

            // Binds
            AnyId::ValueBind(id) => self
                .0
                .value_binds
                .get_unchecked(id)
                .green()
                .display_in(arena),
            AnyId::TypeBind(id) => self
                .0
                .type_binds
                .get_unchecked(id)
                .green()
                .display_in(arena),

            _ => return notation,
        };

        let single = [notation.clone(), arena.notate(" : "), ty.clone()]
            .concat_in(arena)
            .flatten(arena);

        let multi = [notation, arena.newline(), arena.notate(": "), ty]
            .concat_in(arena)
            .indent(arena);

        single.or(multi, arena)
    }
}
