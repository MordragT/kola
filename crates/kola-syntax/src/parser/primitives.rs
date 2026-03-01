use chumsky::prelude::*;

use kola_utils::interner::StrKey;

use super::{KolaParser, State, ext::Captured};
use crate::token::{
    CloseT, Ctrl, CtrlT, Kw, KwT, LiteralT, Op, OpT, OpenT, SemanticToken, Symbol, Token,
};

pub fn op<'t>(op: OpT<'t>) -> Captured<'t, Op, impl KolaParser<'t, Op> + Clone> {
    let parser = just(op.0).to(Op::from(op)).map_with(|op, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(op, span);
        op
    });

    Captured::new(parser)
}

pub fn ctrl<'t>(ctrl: CtrlT<'t>) -> Captured<'t, Ctrl, impl KolaParser<'t, Ctrl> + Clone> {
    let parser = just(ctrl.0).to(Ctrl::from(ctrl)).map_with(|ctrl, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(ctrl, span);
        ctrl
    });

    Captured::new(parser)
}

pub fn kw<'t>(kw: KwT<'t>) -> Captured<'t, Kw, impl KolaParser<'t, Kw> + Clone> {
    let parser = just(kw.0).to(Kw::from(kw)).map_with(|kw, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(kw, span);
        kw
    });

    Captured::new(parser)
}

pub fn open_delim<'t>(
    delim: OpenT<'t>,
) -> Captured<'t, SemanticToken, impl KolaParser<'t, SemanticToken> + Clone> {
    let parser = just(delim.0)
        .to(SemanticToken::from(delim))
        .map_with(|delim, e| {
            let span = e.span();
            let state: &mut State = e.state();

            state.insert_token(delim, span);
            delim
        });

    Captured::new(parser)
}

pub fn close_delim<'t>(
    delim: CloseT<'t>,
) -> Captured<'t, SemanticToken, impl KolaParser<'t, SemanticToken> + Clone> {
    let parser = just(delim.0)
        .to(SemanticToken::from(delim))
        .map_with(|delim, e| {
            let span = e.span();
            let state: &mut State = e.state();

            state.insert_token(delim, span);
            delim
        });

    Captured::new(parser)
}

pub fn literal<'t>() -> impl KolaParser<'t, LiteralT<'t>> + Clone {
    select! { Token::Literal(l) => l }.map_with(|lit, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(lit, span);
        lit
    })
}

pub fn symbol<'t>(st: Symbol) -> impl KolaParser<'t, StrKey> + Clone {
    select! { Token::Symbol(s) => s }.map_with(move |s, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(SemanticToken::Symbol(st), span);
        state.intern(s)
    })
}

pub fn lower_symbol<'t>(st: Symbol) -> impl KolaParser<'t, StrKey> + Clone {
    select! { Token::Symbol(s) if s.starts_with(char::is_lowercase) => s }.map_with(move |s, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(SemanticToken::Symbol(st), span);
        state.intern(s)
    })
}

pub fn upper_symbol<'t>(st: Symbol) -> impl KolaParser<'t, StrKey> + Clone {
    select! { Token::Symbol(s) if s.starts_with(char::is_uppercase) => s }.map_with(move |s, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(SemanticToken::Symbol(st), span);
        state.intern(s)
    })
}
