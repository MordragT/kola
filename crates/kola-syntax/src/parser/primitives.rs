use chumsky::{input::ValueInput, prelude::*};

use super::Extra;
use crate::{
    span::Span,
    token::{CloseT, Ctrl, CtrlT, Kw, KwT, Op, OpT, OpenT, SemanticToken, Token},
};

// TODO See the tokens module for an explanation on how this can enable more features

pub fn op<'t, I>(op: OpT<'t>) -> impl Parser<'t, I, Op, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    just(op.0).to(Op::from(op))
}

pub fn ctrl<'t, I>(ctrl: CtrlT<'t>) -> impl Parser<'t, I, Ctrl, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    just(ctrl.0).to(Ctrl::from(ctrl))
}

pub fn kw<'t, I>(kw: KwT<'t>) -> impl Parser<'t, I, Kw, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    just(kw.0).to(Kw::from(kw))
}

pub fn open_delim<'t, I>(
    delim: OpenT<'t>,
) -> impl Parser<'t, I, SemanticToken<'t>, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    just(delim.0).to(SemanticToken::from(delim))
}

pub fn close_delim<'t, I>(
    delim: CloseT<'t>,
) -> impl Parser<'t, I, SemanticToken<'t>, Extra<'t>> + Clone
where
    I: ValueInput<'t, Token = Token<'t>, Span = Span>,
{
    just(delim.0).to(SemanticToken::from(delim))
}
