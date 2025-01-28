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
        T: InnerNode + Attached<SyntaxPhase, Meta = Span>,
    {
        self.map_with(|node, e| {
            let span = e.span();
            let tree: &mut State = e.state();
            tree.insert(node, span)
        })
    }

    #[inline]
    fn to_expr(self) -> impl Parser<'src, I, NodeId<node::Expr>, Extra<'src>>
    where
        T: InnerNode + Attached<SyntaxPhase, Meta = Span>,
        node::Expr: From<NodeId<T>>,
    {
        self.to_node().map(node::Expr::from).to_node()
    }

    #[inline]
    fn to_pat(self) -> impl Parser<'src, I, node::Pat, Extra<'src>>
    where
        T: Into<node::Pat>,
    {
        self.map(Into::into)
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
