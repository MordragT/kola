use kola_span::combinator::Combinator;
use kola_span::input::Input;
use kola_span::primitive::just;
use kola_span::select;

use super::ParseInput;
use super::state::State;
use crate::token::{
    CloseT, Ctrl, CtrlT, Kw, KwT, LiteralT, Op, OpT, OpenT, SemanticToken, Symbol, Token,
};
use kola_span::primitive::Select;
use kola_utils::interner::StrKey;

pub const fn op<'t>(op: OpT<'t>) -> impl const Combinator<ParseInput<'t>, Op> + Clone {
    just(op.0)
        .to(Op::from(op))
        .map_with(move |op, _loc, input: &mut ParseInput<'t>| {
            let loc = input.prev_loc();
            let state: &mut State = input.state();
            state.insert_token(op, loc);
            op
        })
}

pub const fn ctrl<'t>(ctrl: CtrlT<'t>) -> impl const Combinator<ParseInput<'t>, Ctrl> + Clone {
    just(ctrl.0)
        .to(Ctrl::from(ctrl))
        .map_with(move |ctrl, _loc, input: &mut ParseInput<'t>| {
            let loc = input.prev_loc();
            let state: &mut State = input.state();
            state.insert_token(ctrl, loc);
            ctrl
        })
}

pub const fn kw<'t>(kw: KwT<'t>) -> impl const Combinator<ParseInput<'t>, Kw> + Clone {
    just(kw.0)
        .to(Kw::from(kw))
        .map_with(move |kw, _loc, input: &mut ParseInput<'t>| {
            let loc = input.prev_loc();
            let state: &mut State = input.state();
            state.insert_token(kw, loc);
            kw
        })
}

pub const fn open_delim<'t>(
    delim: OpenT<'t>,
) -> impl const Combinator<ParseInput<'t>, SemanticToken> + Clone {
    just(delim.0).to(SemanticToken::from(delim)).map_with(
        move |delim, _loc, input: &mut ParseInput<'t>| {
            let loc = input.prev_loc();
            let state: &mut State = input.state();
            state.insert_token(delim, loc);
            delim
        },
    )
}

pub const fn close_delim<'t>(
    delim: CloseT<'t>,
) -> impl const Combinator<ParseInput<'t>, SemanticToken> + Clone {
    just(delim.0).to(SemanticToken::from(delim)).map_with(
        move |delim, _loc, input: &mut ParseInput<'t>| {
            let loc = input.prev_loc();
            let state: &mut State = input.state();
            state.insert_token(delim, loc);
            delim
        },
    )
}

pub const fn literal<'t>() -> impl const Combinator<ParseInput<'t>, LiteralT<'t>> + Clone {
    select!(Token::Literal(l) => l).map_with(|lit, _loc, input: &mut ParseInput<'t>| {
        let loc = input.prev_loc();
        let state: &mut State = input.state();
        state.insert_token(lit, loc);
        lit
    })
}

pub const fn symbol<'t>(st: Symbol) -> impl const Combinator<ParseInput<'t>, StrKey> + Clone {
    select!(Token::LowerSymbol(s) => s, Token::UpperSymbol(s) => s).map_with(
        move |s, _loc, input: &mut ParseInput<'t>| {
            let loc = input.prev_loc();
            let state: &mut State = input.state();
            state.insert_token(SemanticToken::Symbol(st), loc);
            state.intern(s)
        },
    )
}

pub const fn lower_symbol<'t>(st: Symbol) -> impl const Combinator<ParseInput<'t>, StrKey> + Clone {
    select!(Token::LowerSymbol(s) => s).map_with(move |s, _loc, input: &mut ParseInput<'t>| {
        let loc = input.prev_loc();
        let state: &mut State = input.state();
        state.insert_token(SemanticToken::Symbol(st), loc);
        state.intern(s)
    })
}

pub const fn upper_symbol<'t>(st: Symbol) -> impl const Combinator<ParseInput<'t>, StrKey> + Clone {
    select!(Token::UpperSymbol(s) => s).map_with(move |s, _loc, input: &mut ParseInput<'t>| {
        let loc = input.prev_loc();
        let state: &mut State = input.state();
        state.insert_token(SemanticToken::Symbol(st), loc);
        state.intern(s)
    })
}
