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
            Meta::ModuleName(_) | Meta::TypeName(_) | Meta::ValueName(_) => return notation,
            // Patterns
            Meta::AnyPat(t)
            | Meta::LiteralPat(t)
            | Meta::IdentPat(t)
            | Meta::RecordPat(t)
            | Meta::VariantPat(t)
            | Meta::Pat(t) => t.green().display_in(arena),
            Meta::RecordFieldPat(p) | Meta::VariantCasePat(p) => return notation,
            Meta::PatError(_) => return notation,

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
            | Meta::CallExpr(t)
            | Meta::Expr(t) => t.green().display_in(arena),
            Meta::RecordField(lt) => return notation,
            Meta::ExprError(_) => return notation,
            Meta::FieldPath(_) => return notation,
            Meta::SelectExpr(_) => todo!(),

            // Types
            Meta::RecordFieldType(lt) | Meta::VariantCaseType(lt) => return notation,
            Meta::RecordType(t) | Meta::VariantType(t) | Meta::FuncType(t) => {
                t.green().display_in(arena)
            }
            Meta::QualifiedType(pt)
            | Meta::TypeApplication(pt)
            | Meta::Type(pt)
            | Meta::TypeScheme(pt) => pt.green().display_in(arena),
            Meta::TypeVar(_) | Meta::TypeError(_) => return notation,

            // Modules
            Meta::TypeBind(pt) => pt.green().display_in(arena),
            Meta::ValueBind(t) => t.green().display_in(arena),
            Meta::Vis(_)
            | Meta::OpaqueTypeBind(_)
            | Meta::ModuleBind(_)
            | Meta::ModuleTypeBind(_)
            | Meta::Bind(_)
            | Meta::Module(_)
            | Meta::ModulePath(_)
            | Meta::ModuleImport(_)
            | Meta::ModuleExpr(_)
            | Meta::ValueSpec(_)
            | Meta::OpaqueTypeKind(_)
            | Meta::OpaqueTypeSpec(_)
            | Meta::ModuleSpec(_)
            | Meta::Spec(_)
            | Meta::ModuleType(_) => return notation,
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
