use std::borrow::Cow;

use kola_interner::{StrInterner, StrKey};
use kola_span::{Loc, Located, Report, ReportCheckpoint};
use kola_tree::prelude::*;
use kola_tree::tree::TotalSpan;

use crate::token::{SemanticToken, SemanticTokens};

#[derive(Debug, Clone, Copy)]
pub struct StateCheckpoint {
    pub tokens: usize,
    pub builder: TreeCheckpoint,
    pub recovered: ReportCheckpoint,
}

#[derive(Debug)]
pub struct State<'t> {
    pub tokens: SemanticTokens,
    pub builder: Tree,
    pub interner: &'t mut StrInterner,
    pub recovered: Report,
}

impl<'t> State<'t> {
    pub fn new(interner: &'t mut StrInterner) -> Self {
        Self {
            tokens: SemanticTokens::default(),
            builder: Tree::default(),
            interner,
            recovered: Report::new(),
        }
    }

    pub fn span<T: 'static>(&self, id: Id<T>) -> Loc
    where
        Tree: TotalSpan<T, Loc>,
    {
        *self.builder.get_span(id)
    }

    pub fn insert<T: 'static>(&mut self, node: T, meta: Loc) -> Id<T>
    where
        Tree: Allocate<Located<T>, Key = T>,
    {
        self.builder.alloc_located((node, meta))
    }

    pub fn insert_as<U: 'static, T: 'static>(&mut self, node: T, meta: Loc) -> Id<U>
    where
        Tree: Allocate<Located<T>, Key = T> + Allocate<Located<U>, Key = U>,
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
            recovered: self.recovered.checkpoint(),
        }
    }

    #[inline]
    pub fn reset(&mut self, cp: StateCheckpoint) {
        self.tokens.truncate(cp.tokens);
        self.builder.restore(&cp.builder);
        self.recovered.reset(cp.recovered);
    }
}
