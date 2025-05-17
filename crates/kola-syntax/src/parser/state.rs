use std::borrow::{Borrow, Cow};

use chumsky::{inspector::Inspector, prelude::*};

use kola_span::Loc;
use kola_tree::prelude::*;
use kola_utils::interner::{StrInterner, StrKey};

use crate::{
    loc::{LocPhase, Locations},
    token::{SemanticToken, SemanticTokens, Token},
};

pub type Error<'t> = Rich<'t, Token<'t>, Loc>;
pub type Extra<'t> = extra::Full<Error<'t>, State<'t>, ()>;

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
}

impl<'t> Borrow<TreeBuilder> for State<'t> {
    fn borrow(&self) -> &TreeBuilder {
        &self.builder
    }
}

impl<'t, I> Inspector<'t, I> for State<'t>
where
    I: Input<'t>,
{
    type Checkpoint = ();
    #[inline(always)]
    fn on_token(&mut self, _token: &<I as Input<'t>>::Token) {}

    #[inline(always)]
    fn on_save<'parse>(&self, _cursor: &chumsky::input::Cursor<'t, 'parse, I>) -> Self::Checkpoint {
    }

    #[inline(always)]
    fn on_rewind<'parse>(
        &mut self,
        _marker: &chumsky::input::Checkpoint<'t, 'parse, I, Self::Checkpoint>,
    ) {
    }
}
