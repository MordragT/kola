use std::borrow::Cow;

use kola_interner::{StrInterner, StrKey};
use kola_span::{Loc, Located, Report, ReportCheckpoint};
use kola_tree::node::ModuleBody;
use kola_tree::prelude::*;
use kola_tree::tree::TotalSpan;

use crate::token::{SemanticToken, SemanticTokens};

#[derive(Debug, Clone, Copy)]
pub struct StateCheckpoint {
    pub tokens: usize,
    pub builder: TreeCheckpoint,
    pub modules: usize,
    pub next_module: u32,
    pub recovered: ReportCheckpoint,
}

#[derive(Debug)]
pub struct State<'t> {
    pub tokens: SemanticTokens,
    pub interner: &'t mut StrInterner,
    pub builder: Tree,
    pub modules: Vec<(ModuleKey, Tree)>,
    pub root_module: ModuleKey,
    pub next_module: u32,
    pub recovered: Report,
}

impl<'t> State<'t> {
    pub fn new(interner: &'t mut StrInterner) -> Self {
        Self {
            tokens: SemanticTokens::default(),
            interner,
            builder: Tree::default(),
            modules: Vec::new(),
            root_module: ModuleKey::new(0),
            next_module: 1,
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
            modules: self.modules.len(),
            next_module: self.next_module,
            recovered: self.recovered.checkpoint(),
        }
    }

    #[inline]
    pub fn reset(&mut self, cp: StateCheckpoint) {
        self.tokens.truncate(cp.tokens);
        self.builder.restore(&cp.builder);
        self.modules.truncate(cp.modules);
        self.next_module = cp.next_module;
        self.recovered.reset(cp.recovered);
    }

    #[inline]
    pub fn next_module_key(&mut self) -> ModuleKey {
        let key = ModuleKey::new(self.next_module);
        self.next_module += 1;
        key
    }

    #[inline]
    pub fn push_module(&mut self, key: ModuleKey, tree: Tree) {
        self.modules.push((key, tree));
    }

    #[inline]
    pub fn finish(self, root: Id<ModuleBody>) -> SourceTrees {
        let mut source = SourceTrees::from_root(self.root_module, self.builder.finish(root));
        for (key, tree) in self.modules {
            source.insert(key, tree);
        }
        source
    }
}
