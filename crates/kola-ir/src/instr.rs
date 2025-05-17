use derive_more::{Display, From};
use kola_print::prelude::*;
use kola_utils::{as_variant, convert::TryAsRef, impl_try_as, interner::StrKey};

use crate::{id::Id, ir::Ir};

// TODO Symbol scoping
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(pub StrKey);

#[derive(Debug, From, Clone, Copy, PartialEq)]
pub enum Instr {
    Symbol(Symbol),
    Atom(Atom),
    Expr(Expr),
}

impl_try_as!(Instr, Symbol(Symbol), Atom(Atom), Expr(Expr));

// impl TryAsRef<Func> for Instr {
//     fn try_as_ref(&self) -> Option<&Func> {
//         as_variant!(self, Self::Atom).and_then(TryAsRef::<Func>::try_as_ref)
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Func {
    pub param: Symbol,
    pub body: Id<Expr>,
}

impl Printable<Ir> for Func {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let param = self.param.display_in(arena);
        let body = self.body.notate(with, arena);

        param.then(body.clone().flatten(arena).or(body, arena), arena)
    }
}

/// An expression is atomic if:
/// - it is guaranteed to terminate
/// - it causes no side effects
/// - it causes no control effects
/// - it never produces an error
#[derive(Debug, From, Clone, Copy, PartialEq)]
pub enum Atom {
    Bool(bool),
    Char(char),
    Num(f64),
    Str(StrKey),
    Func(Func),
    Symbol(Symbol),
}

impl_try_as!(
    Atom,
    Bool(bool),
    Char(char),
    Num(f64),
    Str(StrKey),
    Func(Func),
    Symbol(Symbol)
);

impl Printable<Ir> for Atom {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Bool(b) => b.display_in(arena).enclose_by(arena.just('"'), arena),
            Self::Char(c) => c.display_in(arena).enclose_by(arena.just('"'), arena),
            Self::Num(n) => n.display_in(arena).enclose_by(arena.just('"'), arena),
            Self::Str(s) => s.display_in(arena).enclose_by(arena.just('"'), arena),
            Self::Func(f) => f.notate(with, arena),
            Self::Symbol(s) => s.display_in(arena),
        }
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Less,
    Greater,
    LessEq,
    GreaterEq,
    // Logical
    And,
    Or,
    Xor,
    // Equality
    Eq,
    NotEq,
    // Record
    Merge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RetExpr {
    pub arg: Id<Atom>,
}

impl Printable<Ir> for RetExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let arg = self.arg.notate(with, arena);
        let single = arena.just(' ').then(arg.clone().flatten(arena), arena);
        let multi = arena.newline().then(arg, arena);

        arena.notate("return").then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallExpr {
    pub bind: Symbol,
    pub func: Id<Atom>,
    pub arg: Id<Atom>,
    pub next: Id<Expr>,
}

impl Printable<Ir> for CallExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let func = self.func.notate(with, arena);
        let arg = self.arg.notate(with, arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            func.clone(),
            arena.just('('),
            arg.clone(),
            arena.just(')'),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            func,
            arena.just('('),
            arg,
            arena.just(')'),
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfExpr {
    pub bind: Symbol,
    pub predicate: Id<Atom>,
    pub then: Id<Expr>,
    pub or: Id<Expr>,
    pub next: Id<Expr>,
}

impl Printable<Ir> for IfExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let predicate = self.predicate.notate(with, arena);
        let then = self.then.notate(with, arena);
        let or = self.or.notate(with, arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = if "),
            predicate.clone(),
            arena.notate(" then "),
            then.clone(),
            arena.notate(" else "),
            or.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= if "),
            predicate,
            arena.newline(),
            arena.notate("then "),
            then,
            arena.newline(),
            arena.notate("else "),
            or,
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LetExpr {
    pub bind: Symbol,
    pub value: Id<Atom>,
    pub next: Id<Expr>,
}

impl Printable<Ir> for LetExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let value = self.value.notate(with, arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [bind.clone(), arena.notate(" = "), value.clone()]
            .concat_in(arena)
            .flatten(arena);

        let multi = [bind, arena.newline(), arena.notate("= "), value].concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LetInExpr {
    pub bind: Symbol,
    pub value: Id<Expr>,
    pub inside: Id<Expr>,
    pub next: Id<Expr>,
}

impl Printable<Ir> for LetInExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let value = self.value.notate(with, arena);
        let inside = self.inside.notate(with, arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            value.clone(),
            arena.notate(" in "),
            inside.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            value,
            arena.newline(),
            arena.notate("in "),
            inside,
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnaryExpr {
    pub bind: Symbol,
    pub op: UnaryOp,
    pub arg: Id<Atom>,
    pub next: Id<Expr>,
}

impl Printable<Ir> for UnaryExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let op = self.op.display_in(arena);
        let arg = self.arg.notate(with, arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [bind.clone(), arena.notate(" = "), op.clone(), arg.clone()]
            .concat_in(arena)
            .flatten(arena);

        let multi = [bind, arena.newline(), arena.notate("= "), op, arg].concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BinaryExpr {
    pub bind: Symbol,
    pub op: BinaryOp,
    pub lhs: Id<Atom>,
    pub rhs: Id<Atom>,
    pub next: Id<Expr>,
}

impl Printable<Ir> for BinaryExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let op = self.op.display_in(arena);
        let lhs = self.lhs.notate(with, arena);
        let rhs = self.rhs.notate(with, arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            lhs.clone(),
            op.clone(),
            rhs.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [bind, arena.newline(), arena.notate("= "), lhs, op, rhs].concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

// essentially a linked list
/// An expression is complex if it is not atomic
/// Complex expressions must be in tail position
#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
    Ret(RetExpr),
    Call(CallExpr),
    If(IfExpr),
    Let(LetExpr),
    LetIn(LetInExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    // // Capture a continuation
    // Callcc {
    //     bind: Symbol,
    //     body: InstrId<Expr>,
    //     next: InstrId<Expr>,
    // },

    // // Jump with a value to a continuation
    // Jump {
    //     continuation: InstrId<Atom>,
    //     value: InstrId<Atom>,
    // },
}

impl_try_as!(
    Expr,
    Ret(RetExpr),
    Call(CallExpr),
    If(IfExpr),
    Let(LetExpr),
    LetIn(LetInExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr)
);

impl Printable<Ir> for Expr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Ret(expr) => expr.notate(with, arena),
            Self::Call(expr) => expr.notate(with, arena),
            Self::If(expr) => expr.notate(with, arena),
            Self::Let(expr) => expr.notate(with, arena),
            Self::LetIn(expr) => expr.notate(with, arena),
            Self::Unary(expr) => expr.notate(with, arena),
            Self::Binary(expr) => expr.notate(with, arena),
        }
    }
}
