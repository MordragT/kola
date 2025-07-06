use kola_print::prelude::*;
use kola_tree::{meta::Meta, print::Decorator};

use crate::phase::{ResolvedModule, ResolvedModuleType, ResolvedNodes};

pub struct ResolutionDecorator<'a>(pub &'a ResolvedNodes);

impl<'a> Decorator<'a> for ResolutionDecorator<'a> {
    fn decorate(&self, notation: Notation<'a>, with: usize, arena: &'a Bump) -> Notation<'a> {
        let Some(meta) = self.0.get(&with) else {
            return notation;
        };

        let sym = match meta {
            Meta::QualifiedExpr(resolved) => resolved.red().display_in(arena),
            Meta::LetExpr(value_sym)
            | Meta::LambdaExpr(value_sym)
            | Meta::HandlerClause(value_sym)
            | Meta::BindPat(value_sym)
            | Meta::ListElPat(value_sym)
            | Meta::RecordFieldPat(value_sym)
            | Meta::ValueBind(value_sym) => value_sym.red().display_in(arena),

            Meta::SymbolExpr(any_sym) => any_sym.red().display_in(arena),

            Meta::QualifiedType(resolved) => resolved.red().display_in(arena),
            Meta::TypeVar(type_sym) | Meta::TypeBind(type_sym) | Meta::OpaqueTypeBind(type_sym) => {
                type_sym.red().display_in(arena)
            }

            Meta::QualifiedEffectType(resolved) => resolved.red().display_in(arena),
            Meta::EffectTypeBind(effect_sym) => effect_sym.red().display_in(arena),

            Meta::Module(module_sym)
            | Meta::ModulePath(ResolvedModule(module_sym))
            | Meta::ModuleImport(module_sym)
            | Meta::FunctorApp(module_sym)
            | Meta::ModuleBind(module_sym) => module_sym.red().display_in(arena),

            Meta::ModuleTypeBind(mt_sym)
            | Meta::QualifiedModuleType(ResolvedModuleType(mt_sym)) => {
                mt_sym.red().display_in(arena)
            }
            Meta::FunctorBind(f_sym) => f_sym.red().display_in(arena),
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
