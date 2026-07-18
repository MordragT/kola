use kola_print::prelude::*;
use kola_tree::{node::AnyId, print::Decorator};

use crate::phase::NodeMap;

pub struct ResolutionDecorator<'a>(pub &'a NodeMap);

impl<'a> Decorator<'a> for ResolutionDecorator<'a> {
    fn decorate(&self, notation: Notation<'a>, with: AnyId, arena: &'a Bump) -> Notation<'a> {
        let sym = match with {
            AnyId::BindPat(id) => self.0.bind_pats.get(id).red().display_in(arena),
            AnyId::ListElPat(id) => self.0.list_el_pats.get(id).red().display_in(arena),
            AnyId::RecordFieldPat(id) => self.0.record_field_pats.get(id).red().display_in(arena),
            AnyId::LetExpr(id) => self.0.let_exprs.get(id).red().display_in(arena),
            AnyId::LambdaExpr(id) => self.0.lambda_exprs.get(id).red().display_in(arena),
            AnyId::HandlerClause(id) => self.0.handler_clauses.get(id).red().display_in(arena),
            AnyId::QualifiedType(id) => self.0.qualified_types.get(id).red().display_in(arena),
            AnyId::TypeVar(id) => self.0.type_vars.get(id).red().display_in(arena),
            AnyId::TypeVarBind(id) => self.0.type_var_binds.get(id).red().display_in(arena),
            AnyId::ValueBind(id) => self.0.value_binds.get(id).red().display_in(arena),
            AnyId::TypeBind(id) => self.0.type_binds.get(id).red().display_in(arena),
            AnyId::ModuleBind(id) => self.0.module_binds.get(id).red().display_in(arena),
            AnyId::ModuleTypeBind(id) => self.0.module_type_binds.get(id).red().display_in(arena),
            AnyId::FunctorBind(id) => self.0.functor_binds.get(id).red().display_in(arena),
            AnyId::ModuleBody(id) => self.0.module_bodies.get(id).red().display_in(arena),
            AnyId::ModulePath(id) => self.0.module_paths.get(id).red().display_in(arena),
            AnyId::ModuleImport(id) => self.0.module_imports.get(id).red().display_in(arena),
            AnyId::QualifiedModuleType(id) => self
                .0
                .qualified_module_types
                .get(id)
                .red()
                .display_in(arena),
            _ => return notation,
        };

        let single = [notation.clone(), arena.notate(" @ "), sym.clone()]
            .concat_in(arena)
            .flatten(arena);

        let multi = [notation, arena.newline(), arena.notate("@ "), sym]
            .concat_in(arena)
            .indent(arena);

        single.or(multi, arena)
    }
}
