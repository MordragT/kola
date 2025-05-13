use chumsky::{input::ValueInput, prelude::*};

use kola_span::Loc;
use kola_tree::prelude::*;

use super::{Extra, State};
use crate::{loc::LocPhase, token::Token};

pub trait ParserExt<'t, I, T>: Parser<'t, I, T, Extra<'t>> + Sized
where
    I: ValueInput<'t, Token = Token<'t>, Span = Loc>,
{
    #[inline]
    fn to_node(self) -> impl Parser<'t, I, Id<T>, Extra<'t>>
    where
        Node: From<T>,
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        self.map_with(|node, e| {
            let span = e.span();
            let state: &mut State = e.state();
            state.insert(node, span)
        })
    }

    #[inline]
    fn map_to_node<F, U>(self, f: F) -> impl Parser<'t, I, Id<U>, Extra<'t>>
    where
        F: Fn(T) -> U,
        U: MetaCast<LocPhase, Meta = Loc>,
        Node: From<U>,
    {
        self.map(f).to_node()
    }

    #[inline]
    fn to_expr(self) -> impl Parser<'t, I, Id<node::Expr>, Extra<'t>>
    where
        node::Expr: From<T>,
    {
        self.map(node::Expr::from).to_node()
    }

    #[inline]
    fn to_pat(self) -> impl Parser<'t, I, Id<node::Pat>, Extra<'t>>
    where
        node::Pat: From<T>,
    {
        self.map(node::Pat::from).to_node()
    }

    #[inline]
    fn to_type_expr(self) -> impl Parser<'t, I, Id<node::TypeExpr>, Extra<'t>>
    where
        node::TypeExpr: From<T>,
    {
        self.map(node::TypeExpr::from).to_node()
    }

    #[inline]
    fn to_module_expr(self) -> impl Parser<'t, I, Id<node::ModuleExpr>, Extra<'t>>
    where
        node::ModuleExpr: From<T>,
    {
        self.map(node::ModuleExpr::from).to_node()
    }

    #[inline]
    fn to_bind(self) -> impl Parser<'t, I, Id<node::Bind>, Extra<'t>>
    where
        node::Bind: From<T>,
    {
        self.map(node::Bind::from).to_node()
    }

    #[inline]
    fn to_spec(self) -> impl Parser<'t, I, Id<node::Spec>, Extra<'t>>
    where
        node::Spec: From<T>,
    {
        self.map(node::Spec::from).to_node()
    }
}

impl<
    't,
    T,
    I: ValueInput<'t, Token = Token<'t>, Span = Loc>,
    P: Parser<'t, I, T, Extra<'t>> + Sized,
> ParserExt<'t, I, T> for P
{
}
