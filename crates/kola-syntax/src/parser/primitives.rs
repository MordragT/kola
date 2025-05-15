use chumsky::prelude::*;

use kola_utils::interner::{HasMutStrInterner, StrKey};

use super::{KolaParser, State};
use crate::token::{CloseT, Ctrl, CtrlT, Kw, KwT, LiteralT, Op, OpT, OpenT, SemanticToken, Token};

// TODO See the tokens module for an explanation on how this can enable more features

pub fn op<'t, C>(op: OpT<'t>) -> impl KolaParser<'t, Op, C> + Clone
where
    C: HasMutStrInterner + 't,
{
    just(op.0).to(Op::from(op)).map_with(|op, e| {
        let span = e.span();
        let state: &mut State<C> = e.state();

        state.insert_token(op, span);
        op
    })
}

pub fn ctrl<'t, C>(ctrl: CtrlT<'t>) -> impl KolaParser<'t, Ctrl, C> + Clone
where
    C: HasMutStrInterner + 't,
{
    just(ctrl.0).to(Ctrl::from(ctrl)).map_with(|ctrl, e| {
        let span = e.span();
        let state: &mut State<C> = e.state();

        state.insert_token(ctrl, span);
        ctrl
    })
}

pub fn kw<'t, C>(kw: KwT<'t>) -> impl KolaParser<'t, Kw, C> + Clone
where
    C: HasMutStrInterner + 't,
{
    just(kw.0).to(Kw::from(kw)).map_with(|kw, e| {
        let span = e.span();
        let state: &mut State<C> = e.state();

        state.insert_token(kw, span);
        kw
    })
}

pub fn open_delim<'t, C>(delim: OpenT<'t>) -> impl KolaParser<'t, SemanticToken, C> + Clone
where
    C: HasMutStrInterner + 't,
{
    just(delim.0)
        .to(SemanticToken::from(delim))
        .map_with(|delim, e| {
            let span = e.span();
            let state: &mut State<C> = e.state();

            state.insert_token(delim, span);
            delim
        })
}

pub fn close_delim<'t, C>(delim: CloseT<'t>) -> impl KolaParser<'t, SemanticToken, C> + Clone
where
    C: HasMutStrInterner + 't,
{
    just(delim.0)
        .to(SemanticToken::from(delim))
        .map_with(|delim, e| {
            let span = e.span();
            let state: &mut State<C> = e.state();

            state.insert_token(delim, span);
            delim
        })
}

pub fn literal<'t, C>() -> impl KolaParser<'t, LiteralT<'t>, C> + Clone
where
    C: HasMutStrInterner + 't,
{
    select! { Token::Literal(l) => l }.map_with(|lit, e| {
        let span = e.span();
        let state: &mut State<C> = e.state();

        state.insert_token(lit, span);
        lit
    })
}

pub fn symbol<'t, C>() -> impl KolaParser<'t, StrKey, C> + Clone
where
    C: HasMutStrInterner + 't,
{
    select! { Token::Symbol(s) => s }.map_with(|s, e| {
        let span = e.span();
        let state: &mut State<C> = e.state();

        state.insert_token(SemanticToken::Symbol, span);
        state.intern(s)
    })
}
