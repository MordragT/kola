use chumsky::prelude::*;
use kola_tree::prelude::*;

use crate::{Span, SyntaxPhase, token::Token};

pub type State = extra::SimpleState<StateRepr>;
pub type Extra<'src> = extra::Full<Rich<'src, Token<'src>, Span>, State, ()>;

#[derive(Debug)]
pub struct StateRepr {
    pub builder: TreeBuilder,
    pub meta: Vec<Meta<SyntaxPhase>>,
}

impl StateRepr {
    pub fn new() -> Self {
        Self {
            builder: TreeBuilder::new(),
            meta: Vec::new(),
        }
    }

    pub fn insert<T>(&mut self, node: T, meta: Span) -> NodeId<T>
    where
        T: Into<Node> + Attached<SyntaxPhase, Meta = Span>,
    {
        let id = self.builder.insert::<SyntaxPhase, _>(node);
        self.meta.push(T::into_meta(meta));

        id
    }

    pub fn insert_expr<T>(&mut self, node: T, meta: Span) -> NodeId<node::Expr>
    where
        T: Into<Node> + Attached<SyntaxPhase, Meta = Span>,
        node::Expr: From<NodeId<T>>,
    {
        let id = self.insert(node, meta.clone());
        let expr = node::Expr::from(id);
        self.insert(expr, meta)
    }
}
