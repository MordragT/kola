use std::borrow::Cow;

use chumsky::prelude::*;

use kola_span::Span;
use kola_tree::prelude::*;
use kola_utils::{StrInterner, StrKey};

use crate::{
    span::SpanPhase,
    token::{SemanticToken, SemanticTokens, Token},
};

pub type State = extra::SimpleState<StateRepr>;
pub type Error<'t> = Rich<'t, Token<'t>, Span>;
pub type Extra<'t> = extra::Full<Error<'t>, State, ()>;

#[derive(Debug, Default)]
pub struct StateRepr {
    pub tokens: SemanticTokens,
    pub interner: StrInterner,
    pub builder: TreeBuilder,
    pub meta: Vec<Meta<SpanPhase>>,
}

impl StateRepr {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn span<T>(&self, id: Id<T>) -> Span
    where
        T: MetaCast<SpanPhase, Meta = Span>,
    {
        self.meta.get(id).inner_copied()
    }

    pub fn insert<T>(&mut self, node: T, meta: Span) -> Id<T>
    where
        Node: From<T>,
        T: MetaCast<SpanPhase, Meta = Span>,
    {
        let id = self.builder.insert(node);
        self.meta.push(T::upcast(meta));

        id
    }

    pub fn insert_as<U, T>(&mut self, node: T, meta: Span) -> Id<U>
    where
        Node: From<T> + From<U>,
        T: MetaCast<SpanPhase, Meta = Span>,
        U: From<Id<T>> + MetaCast<SpanPhase, Meta = Span>,
    {
        let id = self.insert(node, meta.clone());
        let u = U::from(id);
        self.insert(u, meta)
    }

    pub fn intern<'a>(&mut self, value: impl Into<Cow<'a, str>>) -> StrKey {
        self.interner.intern(value)
    }

    pub fn insert_token(&mut self, token: impl Into<SemanticToken>, span: Span) {
        self.tokens.push((token.into(), span))
    }
}

impl TreeView for StateRepr {
    fn node<T>(&self, id: Id<T>) -> &T
    where
        Node: kola_utils::TryAsRef<T>,
    {
        self.builder.node(id)
    }

    fn iter_nodes(&self) -> std::slice::Iter<'_, Node> {
        self.builder.iter_nodes()
    }
}
