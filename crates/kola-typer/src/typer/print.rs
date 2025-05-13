use kola_print::prelude::*;
use kola_tree::prelude::*;
use owo_colors::OwoColorize;

use super::TypeInfo;

#[derive(Debug, Clone)]
pub struct TypeDecorator(pub TypeInfo);

impl Decorator for TypeDecorator {
    fn decorate<'a>(
        &'a self,
        notation: Notation<'a>,
        with: Id<()>,
        arena: &'a Bump,
    ) -> Notation<'a> {
        let ty = match self.0.get(with) {
            Meta::Name(_) => return notation,
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
            | Meta::PathExpr(t)
            | Meta::ListExpr(t)
            | Meta::RecordExpr(t)
            | Meta::RecordExtendExpr(t)
            | Meta::RecordRestrictExpr(t)
            | Meta::RecordUpdateOp(t)
            | Meta::RecordUpdateExpr(t)
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
            Meta::RecordField(p) => return notation,
            Meta::ExprError(_) => return notation,

            // Types
            Meta::RecordFieldType(t)
            | Meta::RecordType(t)
            | Meta::VariantCaseType(t)
            | Meta::VariantType(t)
            | Meta::FuncType(t) => t.green().display_in(arena),
            Meta::TypePath(pt)
            | Meta::TypeApplication(pt)
            | Meta::TypeExpr(pt)
            | Meta::Type(pt) => pt.green().display_in(arena),
            Meta::TypeVar(_) | Meta::TypeError(_) => return notation,

            // Modules
            Meta::ValueBind(pt) | Meta::TypeBind(pt) => pt.green().display_in(arena),
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

        let single = [
            notation.clone().flatten(arena),
            arena.notate(" : "),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);
        let multi = [notation, arena.newline(), arena.notate(": "), ty]
            .concat_in(arena)
            .indent(arena);

        single.or(multi, arena)
    }
}
