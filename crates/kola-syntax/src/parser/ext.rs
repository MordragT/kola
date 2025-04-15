use chumsky::{input::ValueInput, prelude::*};
use kola_tree::prelude::*;

use super::{Extra, State};
use crate::{Span, SyntaxPhase, token::Token};

pub trait ParserExt<'src, I, T>: Parser<'src, I, T, Extra<'src>> + Sized
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    #[inline]
    fn to_node(self) -> impl Parser<'src, I, NodeId<T>, Extra<'src>>
    where
        Node: From<T>,
        T: MetaCast<SyntaxPhase, Meta = Span>,
    {
        self.map_with(|node, e| {
            let span = e.span();
            let state: &mut State = e.state();
            state.insert(node, span)
        })
    }

    #[inline]
    fn map_to_node<F, U>(self, f: F) -> impl Parser<'src, I, NodeId<U>, Extra<'src>>
    where
        F: Fn(T) -> U,
        U: MetaCast<SyntaxPhase, Meta = Span>,
        Node: From<U>,
    {
        self.map(f).to_node()
    }

    #[inline]
    fn to_expr(self) -> impl Parser<'src, I, NodeId<node::Expr>, Extra<'src>>
    where
        node::Expr: From<T>,
    {
        self.map(node::Expr::from).to_node()
    }

    #[inline]
    fn to_pat(self) -> impl Parser<'src, I, NodeId<node::Pat>, Extra<'src>>
    where
        node::Pat: From<T>,
    {
        self.map(node::Pat::from).to_node()
    }

    #[inline]
    fn to_type_expr(self) -> impl Parser<'src, I, NodeId<node::TypeExpr>, Extra<'src>>
    where
        node::TypeExpr: From<T>,
    {
        self.map(node::TypeExpr::from).to_node()
    }
}

impl<
    'src,
    T,
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
    P: Parser<'src, I, T, Extra<'src>> + Sized,
> ParserExt<'src, I, T> for P
{
}
