use std::borrow::Cow;

use kola_span::{Loc, Report, ReportCheckpoint};
use kola_tree::{
    node::{StorageCheckpoint, UniversalStorage},
    prelude::*,
};
use kola_utils::interner::{StrInterner, StrKey};

use crate::{
    loc::LocVec,
    token::{SemanticToken, SemanticTokens},
};

#[derive(Debug, Clone, Copy)]
pub struct StateCheckpoint {
    pub tokens: usize,
    pub builder: StorageCheckpoint,
    pub spans: StorageCheckpoint,
    pub recovered: ReportCheckpoint,
}

#[derive(Debug)]
pub struct State<'t> {
    pub tokens: SemanticTokens,
    pub builder: TreeBuilder,
    pub spans: LocVec,
    pub interner: &'t mut StrInterner,
    pub recovered: Report,
}

impl<'t> State<'t> {
    pub fn new(interner: &'t mut StrInterner) -> Self {
        Self {
            tokens: SemanticTokens::default(),
            builder: TreeBuilder::default(),
            spans: LocVec::default(),
            interner,
            recovered: Report::new(),
        }
    }

    pub fn span<T>(&self, id: Id<T>) -> Loc
    where
        UniversalStorage<Loc>: Column<T, Item = Loc>,
    {
        *self.spans.get(id)
    }

    pub fn insert<T>(&mut self, node: T, meta: Loc) -> Id<T>
    where
        NodeStorage: Column<T, Item = T>,
        UniversalStorage<Loc>: Column<T, Item = Loc>,
    {
        let id = self.builder.alloc(node);
        self.spans.vec_mut().push(meta);
        id
    }

    pub fn insert_as<U, T>(&mut self, node: T, meta: Loc) -> Id<U>
    where
        NodeStorage: Column<T, Item = T> + Column<U, Item = U>,
        UniversalStorage<Loc>: Column<T, Item = Loc> + Column<U, Item = Loc>,
        U: From<Id<T>>,
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
            tokens: self.tokens.len(),
            builder: self.builder.checkpoint(),
            spans: self.spans.checkpoint(),
            recovered: self.recovered.checkpoint(),
        }
    }

    #[inline]
    pub fn reset(&mut self, cp: StateCheckpoint) {
        self.tokens.truncate(cp.tokens);
        self.builder.restore(&cp.builder);
        self.spans.restore(&cp.spans);
        self.recovered.reset(cp.recovered);
    }
}
