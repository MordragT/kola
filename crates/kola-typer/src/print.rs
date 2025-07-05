use kola_print::prelude::*;
use kola_tree::prelude::*;
use owo_colors::OwoColorize;

use crate::phase::TypedNodes;

#[derive(Debug, Clone)]
pub struct TypeDecorator<'a>(pub &'a TypedNodes);

impl<'a> Decorator<'a> for TypeDecorator<'a> {
    fn decorate(&self, notation: Notation<'a>, with: usize, arena: &'a Bump) -> Notation<'a> {
        let Some(meta) = self.0.get(&with) else {
            return notation;
        };

        let ty = match meta {
            // Patterns
            Meta::AnyPat(t)
            | Meta::LiteralPat(t)
            | Meta::BindPat(t)
            | Meta::ListElPat(t)
            | Meta::ListPat(t)
            | Meta::RecordPat(t)
            | Meta::VariantPat(t)
            | Meta::Pat(t) => t.green().display_in(arena),
            Meta::RecordFieldPat(lt) | Meta::VariantTagPat(lt) => lt.green().display_in(arena),

            // Expressions
            Meta::LiteralExpr(t)
            | Meta::ListExpr(t)
            | Meta::RecordExpr(t)
            | Meta::RecordExtendExpr(t)
            | Meta::RecordRestrictExpr(t)
            | Meta::RecordUpdateOp(t)
            | Meta::RecordUpdateExpr(t)
            | Meta::QualifiedExpr(t)
            | Meta::UnaryOp(t)
            | Meta::UnaryExpr(t)
            | Meta::BinaryOp(t)
            | Meta::BinaryExpr(t)
            | Meta::LetExpr(t)
            | Meta::CaseBranch(t)
            | Meta::CaseExpr(t)
            | Meta::IfExpr(t)
            | Meta::LambdaExpr(t)
            | Meta::TagExpr(t)
            | Meta::Expr(t)
            | Meta::HandlerClause(t) => t.green().display_in(arena),
            Meta::HandleExpr(ct) | Meta::DoExpr(ct) | Meta::CallExpr(ct) => {
                ct.green().display_in(arena)
            }
            Meta::RecordField(lt) => lt.green().display_in(arena),

            // Effects
            Meta::EffectOpType(lt) => lt.green().display_in(arena),
            Meta::QualifiedEffectType(rt)
            | Meta::EffectRowType(rt)
            | Meta::EffectType(rt)
            | Meta::EffectTypeBind(rt) => rt.green().display_in(arena),

            // Types
            Meta::RecordFieldType(lt) | Meta::TagType(lt) => lt.green().display_in(arena),
            Meta::RecordType(t) | Meta::VariantType(t) | Meta::FuncType(t) => {
                t.green().display_in(arena)
            }
            Meta::CompType(ct) => ct.green().display_in(arena),
            Meta::QualifiedType(pt)
            | Meta::TypeApplication(pt)
            | Meta::Type(pt)
            | Meta::TypeScheme(pt)
            | Meta::TypeVar(pt) => pt.green().display_in(arena),

            // Modules
            Meta::TypeBind(pt) => pt.green().display_in(arena),
            Meta::ValueBind(t) => t.green().display_in(arena),
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
