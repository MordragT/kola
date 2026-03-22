use std::borrow::Cow;

use kola_span::Loc;
use kola_tree::prelude::*;
use kola_utils::interner::{StrInterner, StrKey};

use crate::{
    loc::{LocPhase, Locations},
    token::{SemanticToken, SemanticTokens},
};

#[derive(Debug, Clone, Copy)]
pub struct StateCheckpoint {
    pub tokens_len: usize,
    pub nodes_len: usize,
    pub spans_len: usize,
}

#[derive(Debug)]
pub struct State<'t> {
    pub tokens: SemanticTokens,
    pub builder: TreeBuilder,
    pub spans: Locations,
    pub interner: &'t mut StrInterner,
}

impl<'t> State<'t> {
    pub fn new(interner: &'t mut StrInterner) -> Self {
        Self {
            tokens: SemanticTokens::default(),
            builder: TreeBuilder::default(),
            spans: Locations::default(),
            interner,
        }
    }

    pub fn span<T>(&self, id: Id<T>) -> Loc
    where
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        *self.spans.meta(id)
    }

    pub fn insert<T>(&mut self, node: T, meta: Loc) -> Id<T>
    where
        Node: From<T>,
        T: MetaCast<LocPhase, Meta = Loc>,
    {
        let id = self.builder.insert(node);
        self.spans.push(T::upcast(meta));

        id
    }

    pub fn insert_as<U, T>(&mut self, node: T, meta: Loc) -> Id<U>
    where
        Node: From<T> + From<U>,
        T: MetaCast<LocPhase, Meta = Loc>,
        U: From<Id<T>> + MetaCast<LocPhase, Meta = Loc>,
    {
        let id = self.insert(node, meta.clone());
        let u = U::from(id);
        self.insert(u, meta)
    }

    pub fn intern<'a>(&mut self, value: impl Into<Cow<'a, str>>) -> StrKey {
        self.interner.intern(value)
    }

    pub fn insert_token(&mut self, token: impl Into<SemanticToken>, span: Loc) {
        self.tokens.push((token.into(), span))
    }

    #[inline]
    pub fn checkpoint(&self) -> StateCheckpoint {
        StateCheckpoint {
            tokens_len: self.tokens.len(),
            nodes_len: self.builder.count(),
            spans_len: self.spans.len(),
        }
    }

    #[inline]
    pub fn reset(&mut self, checkpoint: StateCheckpoint) {
        self.tokens.truncate(checkpoint.tokens_len);
        self.builder.truncate(checkpoint.nodes_len);
        self.spans.truncate(checkpoint.spans_len);
    }
}
