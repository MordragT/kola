use chumsky::{input::ValueInput, prelude::*};

use kola_span::Span;
use kola_utils::StrKey;

use super::{Extra, State};
use crate::token::{CloseT, Ctrl, CtrlT, Kw, KwT, LiteralT, Op, OpT, OpenT, SemanticToken, Token};

// TODO See the tokens module for an explanation on how this can enable more features

pub fn op<'t, I>(op: OpT<'t>) -> impl Parser<'t, I, Op, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    just(op.0).to(Op::from(op)).map_with(|op, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(op, span);
        op
    })
}

pub fn ctrl<'t, I>(ctrl: CtrlT<'t>) -> impl Parser<'t, I, Ctrl, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    just(ctrl.0).to(Ctrl::from(ctrl)).map_with(|ctrl, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(ctrl, span);
        ctrl
    })
}

pub fn kw<'t, I>(kw: KwT<'t>) -> impl Parser<'t, I, Kw, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    just(kw.0).to(Kw::from(kw)).map_with(|kw, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(kw, span);
        kw
    })
}

pub fn open_delim<'t, I>(delim: OpenT<'t>) -> impl Parser<'t, I, SemanticToken, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    just(delim.0)
        .to(SemanticToken::from(delim))
        .map_with(|delim, e| {
            let span = e.span();
            let state: &mut State = e.state();

            state.insert_token(delim, span);
            delim
        })
}

pub fn close_delim<'t, I>(delim: CloseT<'t>) -> impl Parser<'t, I, SemanticToken, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    just(delim.0)
        .to(SemanticToken::from(delim))
        .map_with(|delim, e| {
            let span = e.span();
            let state: &mut State = e.state();

            state.insert_token(delim, span);
            delim
        })
}

pub fn literal<'t, I>() -> impl Parser<'t, I, LiteralT<'t>, Extra<'t>> + Sized
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    select! { Token::Literal(l) => l }.map_with(|lit, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(lit, span);
        lit
    })
}

pub fn symbol<'t, I>() -> impl Parser<'t, I, StrKey, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    select! { Token::Symbol(s) => s }.map_with(|s, e| {
        let span = e.span();
        let state: &mut State = e.state();

        state.insert_token(SemanticToken::Symbol, span);
        state.intern(s)
    })
}
