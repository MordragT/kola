use kola_print::prelude::*;
use kola_tree::prelude::*;

use crate::SemanticPhase;

pub type TypeMetadata = Metadata<SemanticPhase>;

#[derive(Debug, Clone)]
pub struct TypeDecorator(pub TypeMetadata);

impl Decorator for TypeDecorator {
    fn decorate<'a>(
        &'a self,
        notation: Notation<'a>,
        with: NodeId<()>,
        arena: &'a Bump,
    ) -> Notation<'a> {
        let ty = match self.0.get(with) {
            Meta::Name(())
            | Meta::Property(())
            | Meta::PatError(())
            | Meta::Branch(())
            | Meta::ExprError(()) => {
                return notation;
            }

            Meta::Ident(t)
            | Meta::Literal(t)
            | Meta::List(t)
            | Meta::Record(t)
            | Meta::RecordSelect(t)
            | Meta::RecordExtend(t)
            | Meta::RecordRestrict(t)
            | Meta::RecordUpdate(t)
            | Meta::UnaryOp(t)
            | Meta::Unary(t)
            | Meta::BinaryOp(t)
            | Meta::Binary(t)
            | Meta::Let(t)
            | Meta::Wildcard(t)
            | Meta::LiteralPat(t)
            | Meta::IdentPat(t)
            | Meta::PropertyPat(t)
            | Meta::RecordPat(t)
            | Meta::Pat(t)
            | Meta::Case(t)
            | Meta::If(t)
            | Meta::Func(t)
            | Meta::Call(t)
            | Meta::Expr(t) => t,
        }
        .display_in(arena);

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
