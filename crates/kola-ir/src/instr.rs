use std::fmt;

use derive_more::{Display, From};
use kola_print::prelude::*;
use kola_utils::{impl_try_as, interner::StrKey};

use crate::{
    id::Id,
    ir::{Ir, IrBuilder},
};

// TODO Symbol scoping
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(pub StrKey);

#[derive(Debug, From, Clone, PartialEq)]
pub enum Instr {
    Symbol(Symbol),
    Atom(Atom),
    Expr(Expr),
    RecordFields(Vec<RecordField>),
}

impl_try_as!(
    Instr,
    Symbol(Symbol),
    Atom(Atom),
    Expr(Expr),
    RecordFields(Vec<RecordField>)
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Func {
    pub param: Symbol,
    pub body: Id<Expr>,
}

impl Func {
    pub fn new(param: Symbol, body: impl Into<Expr>, builder: &mut IrBuilder) -> Self {
        let body = builder.add(body.into());
        Self { param, body }
    }
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

impl RetExpr {
    pub fn new(arg: impl Into<Atom>, builder: &mut IrBuilder) -> Self {
        let arg = builder.add(arg.into());
        Self { arg }
    }
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

impl CallExpr {
    pub fn new(
        bind: Symbol,
        func: impl Into<Atom>,
        arg: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let func = builder.add(func.into());
        let arg = builder.add(arg.into());
        let next = builder.add(next.into());

        Self {
            bind,
            func,
            arg,
            next,
        }
    }
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

impl IfExpr {
    pub fn new(
        bind: Symbol,
        predicate: impl Into<Atom>,
        then: impl Into<Expr>,
        or: impl Into<Expr>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let predicate = builder.add(predicate.into());
        let then = builder.add(then.into());
        let or = builder.add(or.into());
        let next = builder.add(next.into());

        Self {
            bind,
            predicate,
            then,
            or,
            next,
        }
    }
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

impl LetExpr {
    pub fn new(
        bind: Symbol,
        value: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let value = builder.add(value.into());
        let next = builder.add(next.into());

        Self { bind, value, next }
    }
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

impl LetInExpr {
    pub fn new(
        bind: Symbol,
        value: impl Into<Expr>,
        inside: impl Into<Expr>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let value = builder.add(value.into());
        let inside = builder.add(inside.into());
        let next = builder.add(next.into());

        Self {
            bind,
            value,
            inside,
            next,
        }
    }
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

impl UnaryExpr {
    pub fn new(
        bind: Symbol,
        op: UnaryOp,
        arg: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let arg = builder.add(arg.into());
        let next = builder.add(next.into());

        Self {
            bind,
            op,
            arg,
            next,
        }
    }
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

impl BinaryExpr {
    pub fn new(
        bind: Symbol,
        op: BinaryOp,
        lhs: impl Into<Atom>,
        rhs: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let lhs = builder.add(lhs.into());
        let rhs = builder.add(rhs.into());
        let next = builder.add(next.into());

        Self {
            bind,
            op,
            lhs,
            rhs,
            next,
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordField {
    pub label: Symbol,
    pub value: Id<Atom>,
}

impl RecordField {
    pub fn new(label: Symbol, value: impl Into<Atom>, builder: &mut IrBuilder) -> Self {
        let value = builder.add(value.into());
        Self { label, value }
    }
}

impl Printable<Ir> for RecordField {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let label = self.label.display_in(arena);
        let value = self.value.notate(with, arena);

        [label, arena.notate(" = "), value].concat_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordExpr {
    pub bind: Symbol,
    pub fields: Id<Vec<RecordField>>,
    pub next: Id<Expr>,
}

impl RecordExpr {
    pub fn new(
        bind: Symbol,
        fields: impl Into<Vec<RecordField>>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let fields = builder.add(fields.into());
        let next = builder.add(next.into());

        Self { bind, fields, next }
    }
}

impl Printable<Ir> for RecordExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let fields = self.fields.get(with).gather(with, arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            fields.clone().concat_in(arena).flatten(arena),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            fields.concat_in(arena),
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordExtendExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub label: Symbol,
    pub value: Id<Atom>,
    pub next: Id<Expr>,
}

impl RecordExtendExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: Symbol,
        value: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let value = builder.add(value.into());
        let next = builder.add(next.into());

        Self {
            bind,
            base,
            label,
            value,
            next,
        }
    }
}

impl Printable<Ir> for RecordExtendExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let base = self.base.notate(with, arena);
        let label = self.label.display_in(arena);
        let value = self.value.notate(with, arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            base.clone(),
            arena.just('.'),
            label.clone(),
            arena.notate(" = "),
            value.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= +"),
            base,
            arena.just('.'),
            label,
            arena.newline(),
            arena.notate("= "),
            value,
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordRestrictExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub label: Symbol,
    pub next: Id<Expr>,
}

impl RecordRestrictExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: Symbol,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let next = builder.add(next.into());

        Self {
            bind,
            base,
            label,
            next,
        }
    }
}

impl Printable<Ir> for RecordRestrictExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let base = self.base.notate(with, arena);
        let label = self.label.display_in(arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            base.clone(),
            arena.just('.'),
            label.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= -"),
            base,
            arena.just('.'),
            label,
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RecordUpdateOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}

impl fmt::Display for RecordUpdateOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Assign => write!(f, "="),
            Self::AddAssign => write!(f, "+="),
            Self::SubAssign => write!(f, "-="),
            Self::MulAssign => write!(f, "*="),
            Self::DivAssign => write!(f, "/="),
            Self::RemAssign => write!(f, "%="),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordUpdateExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub label: Symbol,
    pub op: RecordUpdateOp,
    pub value: Id<Atom>,
    pub next: Id<Expr>,
}

impl RecordUpdateExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: Symbol,
        op: RecordUpdateOp,
        value: impl Into<Atom>,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let value = builder.add(value.into());
        let next = builder.add(next.into());

        Self {
            bind,
            base,
            label,
            op,
            value,
            next,
        }
    }
}

impl Printable<Ir> for RecordUpdateExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let base = self.base.notate(with, arena);
        let label = self.label.display_in(arena);
        let op = self.op.display_in(arena);
        let value = self.value.notate(with, arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            base.clone(),
            arena.just('.'),
            label.clone(),
            op.clone(),
            value.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            base,
            arena.just('.'),
            label,
            op,
            value,
        ]
        .concat_in(arena);

        single.or(multi, arena).then(next, arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordAccessExpr {
    pub bind: Symbol,
    pub base: Id<Atom>,
    pub label: Symbol,
    pub next: Id<Expr>,
}

impl RecordAccessExpr {
    pub fn new(
        bind: Symbol,
        base: impl Into<Atom>,
        label: Symbol,
        next: impl Into<Expr>,
        builder: &mut IrBuilder,
    ) -> Self {
        let base = builder.add(base.into());
        let next = builder.add(next.into());

        Self {
            bind,
            base,
            label,
            next,
        }
    }
}

impl Printable<Ir> for RecordAccessExpr {
    fn notate<'a>(&'a self, with: &'a Ir, arena: &'a Bump) -> Notation<'a> {
        let bind = self.bind.display_in(arena);
        let base = self.base.notate(with, arena);
        let label = self.label.display_in(arena);
        let next = arena.newline().then(self.next.notate(with, arena), arena);

        let single = [
            bind.clone(),
            arena.notate(" = "),
            base.clone(),
            arena.just('.'),
            label.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            bind,
            arena.newline(),
            arena.notate("= "),
            base,
            arena.just('.'),
            label,
        ]
        .concat_in(arena);

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
    Record(RecordExpr),
    RecordExtend(RecordExtendExpr),
    RecordRestrict(RecordRestrictExpr),
    RecordUpdate(RecordUpdateExpr),
    RecordAccess(RecordAccessExpr),
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
            Self::Record(expr) => expr.notate(with, arena),
            Self::RecordExtend(expr) => expr.notate(with, arena),
            Self::RecordRestrict(expr) => expr.notate(with, arena),
            Self::RecordUpdate(expr) => expr.notate(with, arena),
            Self::RecordAccess(expr) => expr.notate(with, arena),
        }
    }
}
