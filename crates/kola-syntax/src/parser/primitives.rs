use chumsky::prelude::*;

use kola_utils::interner::StrKey;

use super::{KolaParser, State};
use crate::token::{CloseT, Ctrl, CtrlT, Kw, KwT, LiteralT, Op, OpT, OpenT, SemanticToken, Token};

pub fn op<'t>(op: OpT<'t>) -> impl KolaParser<'t, Op> + Clone {
    just(op.0).to(Op::from(op)).map_with(|op, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(op, span);
        op
    })
}

pub fn ctrl<'t>(ctrl: CtrlT<'t>) -> impl KolaParser<'t, Ctrl> + Clone {
    just(ctrl.0).to(Ctrl::from(ctrl)).map_with(|ctrl, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(ctrl, span);
        ctrl
    })
}

pub fn kw<'t>(kw: KwT<'t>) -> impl KolaParser<'t, Kw> + Clone {
    just(kw.0).to(Kw::from(kw)).map_with(|kw, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(kw, span);
        kw
    })
}

pub fn open_delim<'t>(delim: OpenT<'t>) -> impl KolaParser<'t, SemanticToken> + Clone {
    just(delim.0)
        .to(SemanticToken::from(delim))
        .map_with(|delim, e| {
            let span = e.span();
            let state: &mut State = e.state();

            state.insert_token(delim, span);
            delim
        })
}

pub fn close_delim<'t>(delim: CloseT<'t>) -> impl KolaParser<'t, SemanticToken> + Clone {
    just(delim.0)
        .to(SemanticToken::from(delim))
        .map_with(|delim, e| {
            let span = e.span();
            let state: &mut State = e.state();

            state.insert_token(delim, span);
            delim
        })
}

pub fn literal<'t>() -> impl KolaParser<'t, LiteralT<'t>> + Clone {
    select! { Token::Literal(l) => l }.map_with(|lit, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(lit, span);
        lit
    })
}

pub fn symbol<'t>() -> impl KolaParser<'t, StrKey> + Clone {
    select! { Token::Symbol(s) => s }.map_with(|s, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(SemanticToken::Symbol, span);
        state.intern(s)
    })
}

pub fn lower_symbol<'t>() -> impl KolaParser<'t, StrKey> + Clone {
    select! { Token::Symbol(s) if s.starts_with(char::is_lowercase) => s }.map_with(|s, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(SemanticToken::Symbol, span);
        state.intern(s)
    })
}

pub fn upper_symbol<'t>() -> impl KolaParser<'t, StrKey> + Clone {
    select! { Token::Symbol(s) if s.starts_with(char::is_uppercase) => s }.map_with(|s, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(SemanticToken::Symbol, span);
        state.intern(s)
    })
}
