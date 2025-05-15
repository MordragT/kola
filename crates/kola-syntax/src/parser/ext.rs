use chumsky::prelude::*;

use kola_span::Loc;
use kola_tree::prelude::*;
use kola_utils::interner::HasMutStrInterner;

use super::{Extra, ParseInput, State};
use crate::loc::LocPhase;

pub trait KolaParser<'t, T, C>: Parser<'t, ParseInput<'t>, T, Extra<'t, C>> + Sized
where
    C: HasMutStrInterner + 't,
{
    #[inline]
    fn to_node(self) -> impl Parser<'t, ParseInput<'t>, Id<T>, Extra<'t, C>>
    where
        Node: From<T>,
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        self.map_with(|node, e| {
            let span = e.span();
            let state: &mut State<C> = e.state();
            state.insert(node, span)
        })
    }

    #[inline]
    fn map_to_node<F, U>(self, f: F) -> impl Parser<'t, ParseInput<'t>, Id<U>, Extra<'t, C>>
    where
        F: Fn(T) -> U,
        U: MetaCast<LocPhase, Meta = Loc>,
        Node: From<U>,
    {
        self.map(f).to_node()
    }

    #[inline]
    fn to_expr(self) -> impl Parser<'t, ParseInput<'t>, Id<node::Expr>, Extra<'t, C>>
    where
        node::Expr: From<T>,
    {
        self.map(node::Expr::from).to_node()
    }

    #[inline]
    fn to_pat(self) -> impl Parser<'t, ParseInput<'t>, Id<node::Pat>, Extra<'t, C>>
    where
        node::Pat: From<T>,
    {
        self.map(node::Pat::from).to_node()
    }

    #[inline]
    fn to_type_expr(self) -> impl Parser<'t, ParseInput<'t>, Id<node::TypeExpr>, Extra<'t, C>>
    where
        node::TypeExpr: From<T>,
    {
        self.map(node::TypeExpr::from).to_node()
    }

    #[inline]
    fn to_module_expr(self) -> impl Parser<'t, ParseInput<'t>, Id<node::ModuleExpr>, Extra<'t, C>>
    where
        node::ModuleExpr: From<T>,
    {
        self.map(node::ModuleExpr::from).to_node()
    }

    #[inline]
    fn to_bind(self) -> impl Parser<'t, ParseInput<'t>, Id<node::Bind>, Extra<'t, C>>
    where
        node::Bind: From<T>,
    {
        self.map(node::Bind::from).to_node()
    }

    #[inline]
    fn to_spec(self) -> impl Parser<'t, ParseInput<'t>, Id<node::Spec>, Extra<'t, C>>
    where
        node::Spec: From<T>,
    {
        self.map(node::Spec::from).to_node()
    }
}

impl<'t, T, C, P> KolaParser<'t, T, C> for P
where
    P: Parser<'t, ParseInput<'t>, T, Extra<'t, C>> + Sized,
    C: HasMutStrInterner + 't,
{
}
