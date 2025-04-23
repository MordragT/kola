use chumsky::prelude::*;
use kola_tree::prelude::*;

use crate::{Span, SyntaxPhase, token::Token};

pub type State = extra::SimpleState<StateRepr>;
pub type Extra<'src> = extra::Full<Rich<'src, Token<'src>, Span>, State, ()>;

#[derive(Debug, Default)]
pub struct StateRepr {
    pub builder: TreeBuilder,
    pub meta: Vec<Meta<SyntaxPhase>>,
}

impl StateRepr {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert<T>(&mut self, node: T, meta: Span) -> Id<T>
    where
        Node: From<T>,
        T: MetaCast<SyntaxPhase, Meta = Span>,
    {
        let id = self.builder.insert(node);
        self.meta.push(T::upcast(meta));

        id
    }

    pub fn insert_as<U, T>(&mut self, node: T, meta: Span) -> Id<U>
    where
        Node: From<T> + From<U>,
        T: MetaCast<SyntaxPhase, Meta = Span>,
        U: From<Id<T>> + MetaCast<SyntaxPhase, Meta = Span>,
    {
        let id = self.insert(node, meta.clone());
        let u = U::from(id);
        self.insert(u, meta)
    }
}
