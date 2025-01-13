use chumsky::prelude::*;

use crate::node::Node;

use super::{
    ast::{Expr, Pat},
    parser::{Extra, ParserInput},
    Span,
};

// TODO move this somewhere more fitting

pub type SyntaxNode<T> = Node<T, Span>;

pub trait NodeParser<'tokens, 'src: 'tokens, T>:
    Parser<'tokens, ParserInput<'tokens, 'src>, T, Extra<'tokens, 'src>> + Sized
{
    #[inline]
    fn to_node(
        self,
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, SyntaxNode<T>, Extra<'tokens, 'src>> {
        self.map_with(|inner, e| Node::new(inner, e.span()))
    }

    #[inline]
    fn to_expr(
        self,
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Expr<Span>, Extra<'tokens, 'src>>
    where
        SyntaxNode<T>: Into<Expr<Span>>,
    {
        self.to_node().map(Into::into)
    }

    #[inline]
    fn to_pat(
        self,
    ) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Pat<Span>, Extra<'tokens, 'src>>
    where
        SyntaxNode<T>: Into<Pat<Span>>,
    {
        self.to_node().map(Into::into)
    }
}

impl<
        'tokens,
        'src: 'tokens,
        T,
        P: Parser<'tokens, ParserInput<'tokens, 'src>, T, Extra<'tokens, 'src>> + Sized,
    > NodeParser<'tokens, 'src, T> for P
{
}
